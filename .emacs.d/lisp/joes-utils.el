;;; package --- Joe's Utils
;;; Commentary:
;; My custom functions.  Either made by me or stolen from the internet ;)
;;; Code:

(defgroup joe nil
	"My little functions"
	:group 'convenience)

(defcustom
	project-file-extensions
	'("cs" "py" "c" "cpp")
	"File extensions that can be used by find-project function."
	:type 'list)

(defcustom company-capf-prefix-functions '(my-company-capf-prefix)
	"List of functions that return t if current position should skip `company-capf'."
	:type 'list)
(make-variable-buffer-local 'company-capf-prefix-functions)

(defcustom wsl-remote-path "\\\\wsl$\\Arch"
	"Path to wsl remote folder from Windows"
	:type 'string)

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

(defun my-buffer-indentation-offset ()
	(save-excursion
		(if (search-forward-regexp "^\t+[^[:blank:]]" nil t)
			(current-indentation)
			(if (search-forward-regexp "^\s+[^[:blank:]]" nil t)
				(current-indentation)
				tab-width))))

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
								   (lambda (word)
									   (replace-regexp-in-string "\/.+" "" word))
								   (sort
									   (split-string
										   (shell-command-to-string
											   (concat
												   ispell-program-name
												   " dump master "
												   new-dict)))
									   'string-lessp))
					  "\n")))
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
			(indent-for-tab-command)
			(when (and
					  (eq initial-position (point))
					  (eq initial-indentation (current-indentation)))
				(completion-at-point)))))

(defun my-capf-extra-prefix-check (orig-fun command &optional arg &rest _args)
	(when (or
			  (not (eq command 'prefix))
			  (not (seq-some (lambda (func)
								 (not (funcall func)))
					   company-capf-prefix-functions)))
		(funcall orig-fun command arg _args)))

(defun my-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix."
	(unless (or
			  (nth 3 (syntax-ppss))
			  (nth 4 (syntax-ppss)))
		t))

(defun my-tree-sitter-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix in `tree-sitter'."
	(unless
		(save-excursion
			(ignore-errors (backward-char))
			(let* ((cursor (tsc-make-cursor (tree-sitter-node-at-point)))
					  (node (tsc-current-node cursor))
					  (found nil))
				(while (and node (not found))
					(when (or (string-match-p "comment" (pp-to-string (ts-node-type node)))
							  (string-match-p "string" (pp-to-string (ts-node-type node))))
						(setq found t))
					(setq node (tsc-get-parent node)))
				found))
		t))

(defun my-latex-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix in `latex-mode'."
	(let ((start-point (point)))
		(save-excursion
			(and
				(search-backward "\\" nil t 1)
				(not (re-search-forward "}" start-point t 1))
				(or
					(search-forward "{" start-point t 1)
					(not (re-search-forward "[^\\]\\b"
							 (- start-point 1) t 1)))))))

(defun my-create-links-creator-bat-file()
	(interactive)
	(let* ((project-path (read-directory-name "Project root: " "~/")))
		(with-current-buffer
			(find-file-noselect (concat project-path "createlinks.bat"))
			(dolist (original-file-path (directory-files project-path t))
				(when (and (not (string= (substring (file-name-nondirectory original-file-path) 0 1) ".")))
					(let ((file-windows-path
							  (concat wsl-remote-path
								  (replace-regexp-in-string "/" "\\\\" original-file-path))))
						(insert (concat "mklink" (when (car (file-attributes original-file-path)) " /d") " \""
									(file-name-nondirectory original-file-path)
									"\" \""
									file-windows-path "\" \n")))))
			(save-buffer)
			(kill-current-buffer))))

(defun my-lsp-csproj-fix-windows-path ()
	(interactive)
	(let ((root-path (lsp-workspace-root))
			 (perm-excluded recentf-exclude))
		(save-excursion
			(dolist (file (directory-files root-path t ".*\.csproj"))
				(progn
					(find-file file)
					(while (search-forward "c:" nil t)
						(replace-match "/mnt/c" t t))
					(goto-char 0)
					(while (search-forward "\\" nil t)
						(replace-match "/")))
				(save-buffer)
				(kill-current-buffer)))
		(add-to-list 'recentf-exclude ".*csproj")
		(add-to-list 'recentf-exclude ".*sln")
		(recentf-cleanup)
		(setq recentf-exclude perm-excluded)))

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

(provide 'joes-utils)
;;; joes-utils.el ends here
