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

(defcustom company-capf-prefix-functions '(my-company-capf-prefix)
	"Functions that return t if current position should skip `company-capf'."
	:type 'list)
(make-variable-buffer-local 'company-capf-prefix-functions)

(defcustom wsl-remote-path "\\\\wsl$\\Arch"
	"Path to wsl remote folder from Windows"
	:type 'string)

(defcustom wsl-mount-points '(("c:" . "/mnt/c"))
	"WSL mount points for windows drives."
	:type 'alist)

(defcustom wsl-project-path-mapping '()
	"List of key-values of path mapping for project source from wsl to windows."
	:type 'alist)

(defvar dap-session-project-root '())

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

(defun ispell-aspell-words (dict)
	"Return all words of an aspell DICT."
    (string-join
		(cl-mapcar
			(lambda (word)
				(replace-regexp-in-string "\/.+" "" word))
			(sort (split-string
					  (shell-command-to-string
                          (concat "aspell dump master " (car (split-string dict "-")))))
				'string-lessp))
		"\n"))

(defun ispell-hunspell-words (dict)
	(let ((dict-path
			  (car (seq-filter
					   (lambda (path) (string-equal dict (file-name-nondirectory path)))
					   (split-string (shell-command-to-string "hunspell -D"))))))
		(shell-command-to-string (concat "unmunch " dict-path ".dic " dict-path ".aff"))))

(defun ispell-available-dicts ()
	"If hunspell in use, return only found dicts."
	(if ispell-really-hunspell
		(seq-filter (lambda (dict)
						(string-match-p (regexp-quote dict)
							(shell-command-to-string "hunspell -D")))
			(ispell-valid-dictionary-list))
		(ispell-valid-dictionary-list)))

(defun ispell-change-dictionary-and-words ()
	"Switch Ispell dictionary and create words file."
	(interactive)
	(let* ((perm-excluded recentf-exclude)
              (new-dict (completing-read
				            "Use new dictionary: "
				            (mapcar #'list (ispell-available-dicts))
				            nil t))
			  (words
 				  (if ispell-really-hunspell
                      (ispell-hunspell-words new-dict)
                      (ispell-aspell-words new-dict)))
			  (ispell-change-dictionary new-dict))
		(with-current-buffer
			(find-file ispell-complete-word-dict)
			(erase-buffer)
			(insert words)
			(save-buffer)
			(kill-buffer))
        (add-to-list 'recentf-exclude ispell-complete-word-dict)
        (recentf-cleanup)
		(setq recentf-exclude perm-excluded)))

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

(defun find-file-upward (file-name &optional depth)
	"Find FILE-NAME moving up folders up to DEPTH."
    (or depth (setq depth 2))
	(let ((num 0) (file-path "") (cur-path "."))
		(while (< num depth)
			(setq file-path (shell-command-to-string (concat "find " cur-path " -maxdepth 1 -name \"" file-name "\"")))
			(setq cur-path (concat "../" cur-path))
			(setq num (1+ num))
			(when (not (string= file-path ""))
				(setq num 999999)))
		(file-truename file-path)))

(defun tex-compile-update()
	(interactive)
    (require 'tex-mode)
	(when (and (string= (buffer-name) (tex-main-file))
			  (not (string-match-p (regexp-quote "documentclass") (buffer-string))))
		(error "%s" "Main file buffer with documentclass not found. Is it open?"))

	(if (tex-shell-running)
		(tex-kill-job)
		(tex-start-shell))

	(when (< (count-windows) 2)
		(split-window-right))

    (let* ((makefile (find-file-upward "Makefile"))
              (path (file-name-directory makefile)))
        (if (string= makefile "")
            (progn
                ;; No Makefile. Gotta run twice to create table of contents. But first time can be draft mode.
	            (shell-command (concat "lualatex --draftmode --halt-on-error" " " (tex-main-file)))
	            (shell-command (concat "lualatex --halt-on-error" " " (tex-main-file)))
                (setq path "./"))
            (shell-command (concat "cd " path " && make")))
        (let* ((pdf-file-name (replace-regexp-in-string "tex$" "pdf" (tex-main-file)))
			      (pdf-buffer (get-buffer pdf-file-name)))
		    (if pdf-buffer
			    (progn
				    (switch-to-buffer-other-window pdf-buffer)
				    (revert-buffer :noconfirm t))
                (find-file-other-window
                    (car (split-string (shell-command-to-string
                                           (concat "find " path " -name " pdf-file-name)))))))))

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
    (when
		(not (seq-some (lambda (func)
                           (funcall func))
				 company-capf-prefix-functions))
        (apply orig-fun command arg _args)))

(defun my-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix."
    (when (or
              (nth 3 (syntax-ppss))
			  (nth 4 (syntax-ppss)))
		t))

(defun my-latex-company-capf-prefix()
	"Check if current prefix is a valid `company-capf' prefix in `latex-mode'."
    (save-excursion
		(let* ((start-point (point))
			   (pos-slash (search-backward "\\" nil t))
				  (pos-bracket (search-forward "{" nil t)))
			(when (or (not pos-slash)
					  (and pos-bracket
						  (<= pos-bracket start-point)))
                t))))

(defun my-tree-sitter-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix in `tree-sitter'."
	(save-excursion
		(ignore-errors (backward-char))
		(let* ((node-type (tsc-node-type (tree-sitter-node-at-pos :named))))
			(when (or (string-match-p "comment" (pp-to-string node-type))
					  (string-match-p "string" (pp-to-string node-type)))
                t))))

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

(defun my-multi-replace-regexp-in-string (replace-pairs string)
	"Replace in STRING all keys by the values in REPLACE-PAIRS."
	(seq-reduce
		(lambda (string replace-pair)
            (replace-regexp-in-string
                (concat (car replace-pair) "+")
                (cdr replace-pair)
                string
                t))
		replace-pairs
		string))

(defun my-lsp-csproj-copy-windows-files (workspace)
	"Copy all .csproj and .sln from Windows mount to WORKSPACE project root."
	(interactive (list (lsp--read-workspace)))
	(with-lsp-workspace workspace
        (let* ((root-path (lsp-workspace-root))
			      (windows-path (car (rassoc root-path wsl-project-path-mapping))))
		    (dolist (file (directory-files windows-path t ".*\.csproj$"))
			    (copy-file file (concat root-path "/" (file-name-nondirectory file)) t))
		    (dolist (file (directory-files windows-path t ".*\.sln$"))
			    (copy-file file (concat root-path "/" (file-name-nondirectory file)) t)))))

(defun my-lsp-csproj-fix-windows-wsl-path (workspace)
	(interactive (list (lsp--read-workspace)))
    (with-lsp-workspace workspace
        (let ((root-path (lsp-workspace-root))
			     (perm-excluded recentf-exclude))
		    (save-excursion
			    (dolist (file (directory-files root-path t ".*\.csproj"))
				    (progn
					    (find-file file)
					    (dolist (mount wsl-mount-points)
						    (while (search-forward (car mount) nil t)
							    (replace-match (cdr mount) t t))
						    (goto-char 0))
					    (while (search-forward "\\" nil t)
						    (replace-match "/")))
				    (save-buffer)
				    (kill-current-buffer)
				    (add-to-list 'recentf-exclude file)))
		    (recentf-cleanup)
		    (setq recentf-exclude perm-excluded))))

(defun my-lsp-update-csproj (workspace)
	(interactive (list (lsp--read-workspace)))
    (my-lsp-csproj-copy-windows-files workspace)
	(my-lsp-csproj-fix-windows-wsl-path workspace)
	(lsp-workspace-restart workspace))

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
