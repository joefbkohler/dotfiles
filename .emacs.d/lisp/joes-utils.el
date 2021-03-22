;;; package --- Joe's Utils
;;; Commentary:
;; My custom functions.  Either made by me or stolen from the internet ;)
;;; Code:

(defgroup joe nil
	"My little functions"
	:group 'convenience)

(defcustom project-file-extensions
	'("cs" "py" "c" "cpp")
	"File extensions that can be used by find-project function."
	:type 'list)

(defcustom ivy-switch-buffer-major-mode-column 40
	"Column where the Major Mode of the buffer should show in `ivy-switch-buffer'."
	:type 'integer)
(defcustom ivy-switch-buffer-path-column 70
	"Column where the Path of the file visited by the buffer should show in `ivy-switch-buffer'."
	:type 'integer)
(defcustom ivy-counsel-doc-column 40
	"Column where the documentation should show in `counsel'."
	:type 'integer)

(defun blink-minibuffer (&optional time)
	"Blink the minibuffer for a set TIME."
	(unless time (setq time 0.1))
	(invert-face 'mode-line)
	(run-with-timer time nil #'invert-face 'mode-line))

(defun create-scratch-buffer ()
	"Create a new scratch buffer if one does not exist."
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(lisp-interaction-mode))

(defun ispell-change-dictionary-and-words ()
	"Switch Ispell dictionary and create words file."
	(interactive)
	(let* ((new-dict
			   (completing-read
				   "Use new dictionary: "
				   (and (fboundp 'ispell-valid-dictionary-list)
					   (mapcar #'list (ispell-valid-dictionary-list)))
				   nil t))
			  (words

				  (string-join (cl-mapcar
								   (lambda (word) (replace-regexp-in-string "\/.+" "" word))
								   (sort
									   (split-string
										   (shell-command-to-string
											   (concat
												   ispell-program-name
												   " dump master "
												   new-dict)))
									   'string-lessp))
					  "\n")
				  ))
		(ispell-change-dictionary new-dict)
		(with-current-buffer
			(find-file ispell-complete-word-dict)
			(erase-buffer)
			(insert words)
			(save-buffer)
			(kill-buffer))))

(defun find-project()
	"Find the 'first' file recursively with an extesions and opens it using gnu find."
	(interactive)
	(require 'ivy)
	(let ((root-path (read-directory-name "Project root: " "~/"))
	 		 (file-ext (ivy-read "File ext: " project-file-extensions :require-match t)))
		(find-file
			(string-trim
				(shell-command-to-string
					(concat "find " root-path " -iname '*." file-ext "' -print -quit"))))))

(defun tex-compile-update()
	(interactive)
	(when (and (string= (buffer-name) (tex-main-file))
			  (not (string-match-p (regexp-quote "documentclass") (buffer-string))))
		(error "%s" "Main file buffer with documentclass not found. Is it open?"))

	(if (tex-shell-running)
		(tex-kill-job)
		(tex-start-shell))

	(when (< (count-windows) 2)
		(split-window-right))

	;; Gotta run twice to create table of contents. But first time can be draft mode.
	(shell-command (concat "lualatex --draftmode --halt-on-error" " " (tex-main-file)))
	(shell-command (concat "lualatex --halt-on-error" " " (tex-main-file)))

	(let* ((pdf-file-name (replace-regexp-in-string "tex$" "pdf" (tex-main-file)))
			  (pdf-buffer (get-buffer pdf-file-name)))
		(if pdf-buffer
			(progn
				(switch-to-buffer-other-window pdf-buffer)
				(revert-buffer :noconfirm t))
			(find-file-other-window pdf-file-name))))

(defun indent-or-complete ()
	"Try to indent.  If line is already indented, invoke `completion-at-point'."
	(interactive)
	(if mark-active
		(indent-for-tab-command)
		(let ((initial-indentation (current-indentation))
				 (initial-position (point)))
			(indent-according-to-mode)
			(when (and
					  (eq initial-position (point))
					  (eq initial-indentation (current-indentation)))
				(if (featurep 'company)
					(company-complete)
					(completion-at-point))))))

(defun vc-dir-delete-marked-files ()
	"Delete all marked files in a `vc-dir' buffer."
	(interactive)
	(let ((files (vc-dir-marked-files)))
		(if (not files)
			(message "No marked files.")
			(when (yes-or-no-p (format "%s %d marked file(s)? "
								   (if delete-by-moving-to-trash "Trash" "Delete")
								   (length files)))
				(unwind-protect
					(mapcar
						(lambda (path)
							(if (and (file-directory-p path)
									(not (file-symlink-p path)))
								(when (or (not (directory-files
												   path nil directory-files-no-dot-files-regexp))
										  (y-or-n-p
											  (format "Directory `%s' is not empty, really %s? "
												  path (if delete-by-moving-to-trash
														   "trash" "delete"))))
									(delete-directory path t t))
								(delete-file path t)))
						files)
					(revert-buffer))))))

(defun toggle-window-split ()
	(interactive)
	(if (= (count-windows) 2)
		(let* ((this-win-buffer (window-buffer))
				  (next-win-buffer (window-buffer (next-window)))
				  (this-win-edges (window-edges (selected-window)))
				  (next-win-edges (window-edges (next-window)))
				  (this-win-2nd (not (and (<= (car this-win-edges)
											  (car next-win-edges))
										 (<= (cadr this-win-edges)
											 (cadr next-win-edges)))))
				  (splitter
					  (if (= (car this-win-edges)
							  (car (window-edges (next-window))))
						  'split-window-horizontally
						  'split-window-vertically)))
			(delete-other-windows)
			(let ((first-win (selected-window)))
				(funcall splitter)
				(if this-win-2nd (other-window 1))
				(set-window-buffer (selected-window) this-win-buffer)
				(set-window-buffer (next-window) next-win-buffer)
				(select-window first-win)
				(if this-win-2nd (other-window 1))))))

;; Ivy prettify ;)
(defun ivy-switch-buffer-mode-path-transformer (buffer-name)
	"Transformer for `ivy-switch-buffer' that add major mode and path for BUFFER-NAME."
	(let ((buffer (get-buffer buffer-name))
			 (result buffer-name))
		(if buffer
			(let ((buffer-major-mode (string-remove-suffix "-mode" (symbol-name (with-current-buffer buffer major-mode))))
					 (buffer-path (buffer-file-name buffer))
					 (name-size (string-width result)))
				(let ((result (concat result
								  (make-string (max 1 (- ivy-switch-buffer-major-mode-column name-size)) ? )
								  (propertize (concat "" buffer-major-mode) 'face 'font-lock-type-face))))
					(let ((result (concat result
									  (make-string (max 1 (- ivy-switch-buffer-path-column (string-width result))) ? )
									  (propertize (concat "" buffer-path) 'face 'font-lock-comment-face))))
						(truncate-string-to-width result (frame-width) nil nil t t))))
			result)))

(defun ivy-counsel-doc-transformer (symbol-name docstring)
	"Transformer for ivy commands that add SYMBOL-NAME DOCSTRING."
	(truncate-string-to-width
		(car (split-string
				 (concat
					 symbol-name
					 (make-string (max 1 (- ivy-counsel-doc-column (string-width symbol-name))) ? )
					 (propertize docstring 'face 'font-lock-comment-face)) "\n"))
		(frame-width) nil nil t t))

(defun ivy-counsel-function-doc-transformer (function-name)
	"Transformer for ivy commands that add FUNCTION-NAME docstring."
	(let* ((func (car (read-from-string function-name)))
			  (key (where-is-internal func nil t))
			  (name-with-keys
				  (concat function-name
					  (when key
						  (propertize
							  (concat " ["
								  (key-description key)
								  "]")
							  'face 'font-lock-type-face)))))
		(ivy-counsel-doc-transformer name-with-keys
			(concat ""
				(when (functionp func)
					(documentation func))))))

(defun ivy-counsel-variable-doc-transformer (variable-name)
	"Transformer for ivy commands that add VARIABLE-NAME docstring."
	(ivy-counsel-doc-transformer variable-name
		(concat ""
			(documentation-property
				(car (read-from-string variable-name)) 'variable-documentation))))

(provide 'joes-utils)
;;; joes-utils.el ends here
