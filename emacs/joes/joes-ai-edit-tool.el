;;; joes-ai-edit-tool.el --- AI file edit tool and edit review -*- lexical-binding: t; -*-

;;; Commentary:
;; The write_files gptel tool, the base copies that record pending AI edits,
;; the forbidden-form guard applied before any write, and the commands that
;; review, accept, and revert those edits.

;;; Code:

(require 'gptel)
(require 'diff-mode)
(require 'xdg)
(require 'cl-lib)
(require 'seq)

(defconst joes-ai--edits-buffer-name "*AI Edits*"
	"Name of the buffer showing the diff of AI edited files.")

(gptel-make-tool
	:name "write_files"
	:description "Edit or create files by exact text replacement. Each edit replaces the single exact occurrence of text_to_replace in file_path with new_text. text_to_replace must match exactly once (including whitespace and indentation); add surrounding lines to make it unique. To insert, use nearby text as an anchor and repeat it in new_text. To delete text, use an empty new_text. To create a file, use an empty text_to_replace on a non-existent file. Multiple edits on the same file are applied in order. Nothing is changed if any edit fails."
	:category "filesystem"
	:function (lambda (edits)
			(condition-case err
				(progn
				  (joes-ai--write-edits edits)
				  (ignore-errors (joes-ai-show-edits))
				  "Edits applied and saved.")
			  (error (format "Error: %s" (error-message-string err)))))
	:args (list '(:name "edits"
					 :type array
					 :description "List of edits to apply atomically."
					 :items (:type object
								:properties (:file_path
												(:type string
													:description "Absolute path of the file to edit or create.")
												:text_to_replace
												(:type string
													:description "Exact existing text to replace, matching once. Empty to create a new file.")
												:new_text
												(:type string
													:description "Replacement text. Empty to delete text_to_replace."))
								:required ["file_path" "text_to_replace" "new_text"]))))

;; -- Base copies

(defun joes-ai-base-dir ()
	"Return the directory holding the base copies of AI edited files.
Before the write_files tool first edits a file, it copies the file to
`<base-dir>/<absolute path>'."
	(expand-file-name "joes-ai/base/" (xdg-cache-home)))

(defun joes-ai--base-path (path)
	"Return the base copy of PATH, mirrored under `joes-ai-base-dir'."
	(concat (directory-file-name (joes-ai-base-dir)) (expand-file-name path)))

(defun joes-ai--base-target (base)
	"Return the real file path of the BASE copy."
	(substring base (length (directory-file-name (joes-ai-base-dir)))))

;; -- Write path

(defun joes-ai--write-edits (edits)
	"Apply EDITS to their files, saving a base copy of each first.
All edits are applied to work buffers first; files are written only if
every edit succeeds.  On failure, the work buffers are discarded and any
base copy created by this call is deleted."
	(let* ((groups (joes-ai--group-edits edits))
			  (paths (mapcar #'car groups))
			  (works nil)
			  (created nil)
			  (done nil))
		(unless groups
			(error "No edits given"))
		(joes-ai--save-visiting-buffers paths)
		(joes-ai--check-writable paths)
		(unwind-protect
			(progn
				(dolist (group groups)
					(let ((buffer (joes-ai--make-work-buffer (car group))))
						(push (cons (car group) buffer) works)
						(with-current-buffer buffer
							(dolist (edit (cdr group))
								(joes-ai--replace-text (car group)
									(plist-get edit :text_to_replace)
									(plist-get edit :new_text))))))
				(setq works (nreverse works))
				(dolist (path paths)
					(let ((base (joes-ai--base-path path)))
						(unless (file-exists-p base)
							(push path created)
							(make-directory (file-name-directory base) t)
							(if (file-exists-p path)
								(copy-file path base nil t)
								(write-region "" nil base nil 'silent)))))
				(joes-ai--guard-work-buffers works)
				(pcase-dolist (`(,path . ,buffer) works)
					(make-directory (file-name-directory path) t)
					(with-current-buffer buffer
						(write-region nil nil path nil 'silent)))
				(dolist (path paths)
					(with-demoted-errors "joes-ai: %S"
						(joes-ai--sync-visiting-buffer path)))
				(setq done t))
			(pcase-dolist (`(,_ . ,buffer) works)
				(kill-buffer buffer))
			(unless done
				(dolist (path created)
					(ignore-errors (delete-file (joes-ai--base-path path))))))))

(defun joes-ai--group-edits (edits)
	"Group EDITS by absolute file path, preserving order.
Return a list of (PATH EDIT...)."
	(let (groups)
		(seq-doseq (edit edits)
			(let* ((path (expand-file-name (plist-get edit :file_path)))
				  (cell (assoc path groups)))
				(unless cell
					(setq cell (list path))
					(push cell groups))
				(setcdr cell (append (cdr cell) (list edit)))))
		(nreverse groups)))

(defun joes-ai--check-writable (paths)
	"Signal an error listing PATHS that cannot be written or created."
	(when-let* ((bad (seq-remove
					  (lambda (path)
						  (if (file-exists-p path)
							  (file-writable-p path)
							  (let ((dir (file-name-directory (directory-file-name (expand-file-name path)))))
								  (while (not (file-exists-p dir))
									  (setq dir (file-name-directory (directory-file-name dir))))
								  (and (file-directory-p dir) (file-writable-p dir)))))
					  paths)))
		(error "Not writable: %s; no files changed" (string-join bad ", "))))

(defun joes-ai--save-visiting-buffers (paths)
	"Save every modified buffer visiting one of PATHS.
Run before any base copy or work buffer is made, so the saved text is what
the edits and the base copy are based on."
	(dolist (path paths)
		(when-let* ((buf (find-buffer-visiting path)))
			(when (buffer-modified-p buf)
				(with-current-buffer buf
					(save-buffer))))))

(defun joes-ai--sync-visiting-buffer (path)
	"Revert the buffer visiting PATH, or kill it if PATH no longer exists."
	(when-let* ((buf (find-buffer-visiting path)))
		(with-current-buffer buf
			(if (file-exists-p path)
				(revert-buffer t t t)
				(set-buffer-modified-p nil)
				(kill-buffer buf)))))

(defun joes-ai--make-work-buffer (path)
	"Return a new buffer holding the contents of PATH.
The buffer is empty if PATH does not exist."
	(let ((buffer (generate-new-buffer " *joes-ai-work*")))
		(with-current-buffer buffer
			(when (file-exists-p path)
				(insert-file-contents path)
				(setq-local buffer-file-coding-system last-coding-system-used)))
		buffer))

(defun joes-ai--replace-text (path old new)
	"Replace the unique occurrence of OLD with NEW in the current buffer.
An empty OLD inserts NEW, and is only allowed if the buffer is empty.
PATH is used in error messages."
	(let ((case-fold-search nil)
			 (new (or new "")))
		(goto-char (point-min))
		(if (or (null old) (string-empty-p old))
			(if (> (buffer-size) 0)
				(error "%s: empty text_to_replace is only allowed for new or empty files" path)
				(insert new))
			(unless (search-forward old nil t)
				(error "%s: text_to_replace not found: %S" path
					(truncate-string-to-width old 80 nil nil "...")))
			(let ((beg (match-beginning 0))
					 (end (match-end 0)))
				(goto-char (1+ beg))
				(when (search-forward old nil t)
					(error "%s: text_to_replace matches more than once, add context: %S" path
						(truncate-string-to-width old 80 nil nil "...")))
				(delete-region beg end)
				(goto-char beg)
				(insert new)))))

;; -- Forbidden-form guard

(defconst joes-ai--elisp-guard-forbidden-heads
	'(defmacro cl-defmacro eval-when-compile eval-and-compile cl-eval-when static-if load)
	"Heads of forms that define macros or run code at compile time.")

(defun joes-ai--forbidden-form-p (form)
	"Return non-nil if FORM, or any subform, is forbidden."
	(and (consp form)
		(or (memq (car form) joes-ai--elisp-guard-forbidden-heads)
			(let ((tail form)
				  (found nil))
				(while (and (consp tail) (not found))
					(setq found (joes-ai--forbidden-form-p (car tail))
						  tail (cdr tail)))
				found))))

(defun joes-ai--forbidden-toplevel-forms (string)
	"Return each top-level form in STRING that contains a forbidden form.
The forms are returned in order in text format."
	(let ((result nil))
		(with-temp-buffer
			(insert string)
			(goto-char (point-min))
			(condition-case nil
				(while t
					(let ((form (read (current-buffer))))
						(when (joes-ai--forbidden-form-p form)
							(push (prin1-to-string form) result))))
				(end-of-file nil)))
		(nreverse result)))

(defun joes-ai--guard-work-buffers (works)
	"Signal an error if a .el work buffer changes a forbidden top-level form.
WORKS is an alist of (PATH . WORK-BUFFER).	The baseline for each path is its
base copy, which holds the content from before the pending edits.  A top-level
form that contains a forbidden form must match the baseline in count, order,
and text.  Adding, removing, moving, duplicating, or editing such a form is
refused.  Other forms are not checked.	A new file's base copy is empty, so any
forbidden form in it is refused."
	(pcase-dolist (`(,path . ,buffer) works)
		(when (string-suffix-p ".el" path)
			(let* ((base (joes-ai--base-path path))
					(source (cond ((file-exists-p base) base)
								  ((file-exists-p path) path)))
					(baseline (if source
								  (with-temp-buffer
									  (insert-file-contents source)
									  (buffer-string))
								"")))
				(unless (equal (joes-ai--forbidden-toplevel-forms baseline)
							   (joes-ai--forbidden-toplevel-forms
								(with-current-buffer buffer (buffer-string))))
					(error "Guard: %s changes a top-level form containing a forbidden form; no files changed"
						path))))))

(defun joes-ai--guard-elisp-form (form)
	"Signal an error if FORM, or any subform, has a forbidden head."
	(when (consp form)
		(let ((head (car form)))
			(when (memq head joes-ai--elisp-guard-forbidden-heads)
				(error "Guard: forbidden form `%s'" head)))
		(let ((rest form))
			(while (consp rest)
				(joes-ai--guard-elisp-form (car rest))
				(setq rest (cdr rest))))))

;; -- Review commands

(defun joes-ai-show-edits ()
	"Show the diff of every base file against its current file."
	(interactive)
	(let ((dir (joes-ai-base-dir))
			 (buf (get-buffer-create joes-ai--edits-buffer-name)))
		(with-current-buffer buf
			(let ((inhibit-read-only t))
				(erase-buffer)
				(when (file-directory-p dir)
					(dolist (base (directory-files-recursively dir ""))
						(let ((path (joes-ai--base-target base))
								 (text nil))
							(with-temp-buffer
								(diff-no-select base path '("-u" "-N") t (current-buffer))
								(goto-char (point-min))
								(when (re-search-forward "^--- " nil t)
									(let ((beg (match-beginning 0)))
										;; Drop the trailing "Diff finished" line and its blank line.
										(when (re-search-forward "^\n?Diff finished" nil t)
											(delete-region (match-beginning 0) (point-max)))
										;; Replace the base and temp file names in the headers with PATH.
										(goto-char beg)
										(delete-region (point) (line-end-position))
										(insert "--- " path)
										(forward-line 1)
										(delete-region (point) (line-end-position))
										(insert "+++ " path)
										(setq text (concat "diff " path "\n"
													   (buffer-substring-no-properties beg (point-max)))))))
							(when text
								(insert text)))))
				(diff-mode)
				(read-only-mode 1)
				(joes-ai-edits-mode 1)
				(goto-char (point-min))))
		(pop-to-buffer buf)))

(defun joes-ai-accept-all-edits ()
	"Accept the AI edits by deleting all base files."
	(interactive)
	(when (y-or-n-p "Accept AI edits and delete their base files? ")
		(let ((dir (joes-ai-base-dir)))
			(when (file-directory-p dir)
				(delete-directory dir t)))
		(when-let* ((buf (get-buffer joes-ai--edits-buffer-name)))
			(delete-windows-on buf t)
			(kill-buffer buf))
		(message "AI edits accepted.")))

(defun joes-ai-accept-edit-hunk ()
	"Accept the AI edit hunk at point.
The hunk is applied to the base copy of its file with,
then the edits buffer is refreshed."
	(interactive)
	(unless (derived-mode-p 'diff-mode)
		(user-error "Not in an AI edits buffer"))
	(let* ((line (line-number-at-pos))
			(path (save-excursion
					(diff-beginning-of-hunk t)
					(unless (re-search-backward "^diff \\(.+\\)$" nil t)
						(user-error "No file header above hunk"))
					(match-string-no-properties 1)))
			(base (joes-ai--base-path path))
			(hunk (save-excursion
					(diff-beginning-of-hunk t)
					(let ((beg (point)))
						(buffer-substring-no-properties beg (diff-end-of-hunk))))))
		(unless (file-exists-p base)
			(user-error "No base copy for %s" path))
		(with-temp-buffer
			(insert (format "--- %s\n+++ %s\n" base base) hunk)
			(diff-mode)
			(goto-char (point-min))
			(forward-line 2)
			(let ((diff-advance-after-apply-hunk nil))
				(diff-apply-hunk)))
		(when-let* ((buf (find-buffer-visiting base)))
			(with-current-buffer buf
				(save-buffer)))
		(when (and (file-exists-p path)
				   (equal (with-temp-buffer
							  (insert-file-contents-literally base)
							  (buffer-string))
						  (with-temp-buffer
							  (insert-file-contents-literally path)
							  (buffer-string))))
			(delete-file base))
		(when-let* ((buf (find-buffer-visiting base)))
			(kill-buffer buf))
		(joes-ai-show-edits)
		(goto-char (point-min))
		(forward-line (1- line))))

(defun joes-ai-revert-edit-hunk ()
	"Revert the AI edit hunk at point, or the hunks in the region.
The hunk is reverse-applied to its file, the file is saved, and the
edits buffer is refreshed."
	(interactive)
	(let ((line (line-number-at-pos)))
		(call-interactively #'diff-revert-and-kill-hunk)
		(joes-ai-show-edits)
		(goto-char (point-min))
		(forward-line (1- line))))

(defvar-keymap joes-ai-edits-mode-map
	:doc "Keymap for `joes-ai-edits-mode'.	Keys are set in `joes-keybinding-ai'.")

(define-minor-mode joes-ai-edits-mode
	"Minor mode for the AI edits diff buffer."
	:lighter " AI-Edits"
	:keymap joes-ai-edits-mode-map
	(when joes-ai-edits-mode
		(setq-local outline-minor-mode-cycle t)
		(outline-minor-mode 1)))

(defun joes-ai--pending-features ()
	"Return the feature symbols named by the pending .el files.
The feature name is assumed to equal the file base name."
	(delete-dups
		(mapcar (lambda (path) (intern (file-name-sans-extension (file-name-nondirectory path))))
			(seq-filter (lambda (path) (string-suffix-p ".el" path))
				(let ((dir (joes-ai-base-dir)))
					(when (file-directory-p dir)
						(mapcar #'joes-ai--base-target
							(seq-filter #'file-regular-p
								(directory-files-recursively dir "")))))))))


;; Flymake byte-compiles in a child Emacs that never loads this file. The
;; child is started with a --eval that marks the pending features as provided,
;; so `require' returns without loading them. Only the child started by
;; `elisp-flymake-byte-compile' (process name below) is changed.
(defun joes-ai--flymake-inject-features (orig &rest args)
	"Run ORIG with ARGS, adding a --eval to the Flymake child command.
The --eval marks every pending feature as provided.	 Intended as `:around'
advice on `make-process'."
	(let ((cmd (plist-get args :command)))
		(if (and (equal (plist-get args :name) "elisp-flymake-byte-compile")
				 (member "--batch" cmd))
			(let* ((pos (1+ (cl-position "--batch" cmd :test #'equal)))
				   (eval-form (format "(dolist (f '%S) (add-to-list 'features f))"
									(joes-ai--pending-features))))
				(apply orig (plist-put args :command
									   (append (seq-take cmd pos)
											   (list "--eval" eval-form)
											   (seq-drop cmd pos)))))
			(apply orig args))))

;; Trampolines for advised primitives are generated on the fly by native
;; compilation, which fails with "call-process invoked recursively" when a
;; primitive is advised from a compiling Emacs.	 Disable them for this install.
(let ((native-comp-enable-subr-trampolines nil))
	(advice-add 'make-process :around #'joes-ai--flymake-inject-features))

(provide 'joes-ai-edit-tool)
;;; joes-ai-edit-tool.el ends here.
