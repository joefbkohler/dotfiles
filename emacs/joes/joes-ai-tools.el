;;; joes-ai-tools.el --- AI assistant tools	 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joe Köhler

;; Author: Joe Köhler <joe.fb.kohler@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'gptel)
(require 'joes-utils)
(require 'diff-mode)
(require 'xdg)

(defcustom joes-ai-read-url-max-chars 16000
	"Maximum characters returned by the read_url tool.
Avoids context overloading.	 nil means no limit."
	:group 'joe
	:type '(choice (const :tag "No limit" nil) integer))

(defconst joes-ai--edits-buffer-name "*AI Edits*"
	"Name of the buffer showing the diff of AI edited files.")

(gptel-make-tool
	:name "search_web"
	:description "Search web for the given string. Returns a list of urls with a short description."
	:category "web"
	:async t
	:function (lambda (callback search-string)
				  (url-retrieve (concat "https://duckduckgo.com/html/?q=" search-string)
					  (lambda (_)
						  (funcall callback (joes-get-list-of-ddg-results (buffer-string)))
						  (kill-buffer (current-buffer)))
					  nil t t))
	:args (list '(:name "search-string"
					 :type string
					 :description "The string used to search the web.")))

(gptel-make-tool
	:name "read_url"
	:description "Returns the content of a web page."
	:category "web"
	:async t
	:function (lambda (ai-callback url)
				  (url-retrieve url
					  (lambda (_)
						  (re-search-forward "<head[^>]*>" nil t)
						  (let ((html-tree (libxml-parse-html-region (point) (point-max)))
								   (tag-to-remove '(style script nav img svg form button)))

							  (dolist (tag tag-to-remove)
								  (dolist (node (dom-by-tag html-tree tag))
									  (dom-remove-node html-tree node)))

							  (with-temp-buffer
								  (dom-print html-tree)
								  (let ((html-text (replace-regexp-in-string ">[[:space:]]+" " " (buffer-substring-no-properties (point-min) (point-max)))))
									  (funcall ai-callback
										  (if (or (null joes-ai-read-url-max-chars)
												  (length< html-text joes-ai-read-url-max-chars))
											  html-text
											  (format "The content of the web page is too long: %i. Use local retriever." (length html-text)))))))
						  (kill-buffer (current-buffer)))
					  nil t t))
	:args (list '(:name "url"
					 :type string
					 :description "The url of the web page to open.")))

(gptel-make-tool
	:name "find_files"
	:description "Find files whose names match a regexp under a directory. Returns a list of absolute file paths."
	:category "filesystem"
	:function (lambda (directory regexp)
				  (let ((files (directory-files-recursively
								   (expand-file-name directory) regexp)))
					  (mapconcat #'identity files "\n")))
	:args (list '(:name "directory"
					 :type string
					 :description "Directory to search in.")
			  '(:name "regexp"
				   :type string
				   :description "Regexp matched against file names.")))

(gptel-make-tool
	:name "rgrep"
	:description "Search files under a directory for a regexp. Returns matching lines as file:line:text."
	:category "filesystem"
	:async t
	:function (lambda (callback directory regexp file-glob)
				  (let ((default-directory (expand-file-name directory)))
					  (save-window-excursion
						  (joes-compilation-buffer-string-async callback "*grep*")
						  (rgrep regexp file-glob default-directory))))
	:args (list '(:name "directory"
					 :type string
					 :description "Directory to search in.")
			  '(:name "regexp"
				   :type string
				   :description "Regexp to search for in file contents.")
			  '(:name "file-glob"
				   :type string
				   :description "Glob matched against file names to include, e.g. \"*.el\".")))

(gptel-make-tool
	:name "read_file"
	:description "Visit a file and return its contents. If start-line and end-line are given (1-based, inclusive), return only those lines."
	:category "filesystem"
	:function (lambda (file-path &optional start-line end-line)
				  (with-current-buffer (find-file-noselect (expand-file-name file-path))
					  (if (not (or start-line end-line))
						  (buffer-substring-no-properties (point-min) (point-max))
						  (let ((start-point (save-excursion
												 (goto-char (point-min))
												 (forward-line (1- (or start-line 1)))
												 (point)))
								   (end-point (if end-line
												  (save-excursion
													  (goto-char (point-min))
													  (forward-line (1- end-line))
													  (line-end-position))
												  (point-max))))
							  (buffer-substring-no-properties start-point end-point)))))
	:args (list '(:name "file-path"
					 :type string
					 :description "Path of the file to read.")
			  '(:name "start-line"
				   :type integer
				   :optional t
				   :description "First line to return, 1-based.")
			  '(:name "end-line"
				   :type integer
				   :optional t
				   :description "Last line to return, 1-based and inclusive.")))

(gptel-make-tool
	:name "write_files"
	:description "Edit or create files by exact text replacement. Each edit replaces the single exact occurrence of text_to_replace in file_path with new_text. text_to_replace must match exactly once (including whitespace and indentation); add surrounding lines to make it unique. To insert, use nearby text as an anchor and repeat it in new_text. To delete, use an empty new_text. To create a file, use an empty text_to_replace on a non-existent file. Multiple edits on the same file are applied in order. Nothing is changed if any edit fails."
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

(gptel-make-tool
	:name "emacs_function_definition"
	:description "Return the whole body of an elisp function"
	:category "emacs"
	:function (lambda (function-name)
				  (let ((function-symbol (intern function-name)))
					  (if (fboundp function-symbol)
						  (let* ((buffer-and-point (find-function-noselect function-symbol))
									(buffer (car buffer-and-point))
									(point (cdr buffer-and-point)))
							  (with-current-buffer buffer
								  (goto-char point)
								  (let ((start (line-number-at-pos)))
									  (end-of-defun)
									  (buffer-substring-no-properties start (point)))))
						  "Function doesn't exist or not loaded")))
	:args (list '(:name "function-name"
					 :type string
					 :description "Name of the function")))

(gptel-make-tool
	:name "emacs_describe_symbol"
	:description "Return the documentation of an Emacs Lisp function or variable."
	:category "emacs"
	:function (lambda (name)
				  (let ((symbol (intern-soft name)))
					  (or (cond
						  ((null symbol) (format "No symbol named %s" name))
						  ((fboundp symbol) (documentation symbol t))
						  ((boundp symbol)
							  (documentation-property symbol 'variable-documentation t)))
						  (format "No documentation for %s" name))))
	:args (list '(:name "symbol"
					 :type string
					 :description "Name of the function or variable to describe.")))

;; -- Helper functions

(defun joes-ai--write-edits (edits)
	"Apply EDITS to their files, saving a base copy of each first.
All edits are applied to work buffers first; files are written only if
every edit succeeds.  On failure, the work buffers are discarded."
	(let* ((groups (joes-ai--group-edits edits))
			  (paths (mapcar #'car groups))
			  (works nil))
		(unless groups
			(error "No edits given"))
		(joes-ai--check-visiting-buffers paths)
		(joes-ai--check-writable paths)
		(unwind-protect
			(progn
				(dolist (group groups)
					(let ((buffer (joes-ai--make-work-buffer (car group))))
						(push (cons (car group) buffer) works)
						(joes-ai--apply-edits buffer (car group) (cdr group))))
				(setq works (nreverse works))
				(dolist (path paths)
					(joes-ai--ensure-base path))
				(pcase-dolist (`(,path . ,buffer) works)
					(joes-ai--write-work-buffer buffer path))
				(dolist (path paths)
					(with-demoted-errors "joes-ai: %S"
						(joes-ai--sync-visiting-buffer path))))
			(pcase-dolist (`(,_ . ,buffer) works)
				(kill-buffer buffer)))))

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

(defun joes-ai--make-work-buffer (path)
	"Return a new buffer holding the contents of PATH.
The buffer is empty if PATH does not exist."
	(let ((buffer (generate-new-buffer " *joes-ai-work*")))
		(with-current-buffer buffer
			(when (file-exists-p path)
				(insert-file-contents path)
				(setq-local buffer-file-coding-system last-coding-system-used)))
		buffer))

(defun joes-ai--apply-edits (buffer path edits)
	"Apply EDITS, in order, to BUFFER holding PATH."
	(with-current-buffer buffer
		(dolist (edit edits)
			(joes-ai--replace-text path
				(plist-get edit :text_to_replace)
				(plist-get edit :new_text)))))

(defun joes-ai--ensure-base (path)
	"Copy PATH to its base unless the base exists.
An empty base is created when PATH does not exist."
	(let ((base (joes-ai--base-path path)))
		(unless (file-exists-p base)
			(make-directory (file-name-directory base) t)
			(if (file-exists-p path)
				(copy-file path base nil t)
				(write-region "" nil base nil 'silent)))))

(defun joes-ai--write-work-buffer (buffer path)
	"Write BUFFER to PATH, creating its parent directories."
	(make-directory (file-name-directory path) t)
	(with-current-buffer buffer
		(write-region nil nil path nil 'silent)))

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

(defun joes-ai--nearest-existing-ancestor (path)
	"Return the closest existing ancestor directory of PATH."
	(let ((dir (file-name-directory (directory-file-name (expand-file-name path)))))
		(while (not (file-exists-p dir))
			(setq dir (file-name-directory (directory-file-name dir))))
		dir))

(defun joes-ai--writable-p (path)
	"Return non-nil if PATH can be written, or created when it does not exist."
	(if (file-exists-p path)
		(file-writable-p path)
		(let ((dir (joes-ai--nearest-existing-ancestor path)))
			(and (file-directory-p dir) (file-writable-p dir)))))

(defun joes-ai--check-writable (paths)
	"Signal an error listing PATHS that cannot be written or created."
	(when-let* ((bad (seq-remove #'joes-ai--writable-p paths)))
		(error "Not writable: %s; no files changed" (string-join bad ", "))))

(defun joes-ai--check-visiting-buffers (paths)
	"Signal an error if any of PATHS is visited by a modified buffer.
Run before any file is touched."
	(dolist (path paths)
		(when-let* ((buf (find-buffer-visiting path)))
			(when (buffer-modified-p buf)
				(error "%s has unsaved changes in buffer %s; no files changed"
					path (buffer-name buf))))))

(defun joes-ai--sync-visiting-buffer (path)
	"Revert the buffer visiting PATH, or kill it if PATH no longer exists."
	(when-let* ((buf (find-buffer-visiting path)))
		(with-current-buffer buf
			(if (file-exists-p path)
				(revert-buffer t t t)
				(set-buffer-modified-p nil)
				(kill-buffer buf)))))

;; AI Edit functions

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

(defun joes-ai-accept-edits ()
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

(provide 'joes-ai-tools)
;;; joes-ai-tools.el ends here.
