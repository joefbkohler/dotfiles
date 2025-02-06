;;; package --- Joe's Unity Development in WSL functions
;;; Commentary:
;; Functions and configurations for Unity Engine development using WSL.
;;; Code:

(require 'joes-utils)
(require 'whitespace)
(require 'eglot)
(require 'recentf)

(defgroup joe-wsl nil
	"My little functions."
	:group 'convenience)

(defcustom framework-path-override "/usr/lib/mono"
	"Path to .NETFramework."
	:type 'string)

(defcustom wsl-remote-path "\\\\wsl$\\Arch"
	"Path to wsl remote folder from Windows"
	:type 'string)

(defcustom wsl-mount-points '(("c:" . "/mnt/c"))
	"WSL mount points for windows drives."
	:type 'alist)

(defcustom wsl-project-path-mapping '()
	"List of key-values of path mapping for project source from wsl to windows."
	:type 'alist)

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
	(eglot-reconnect (eglot-current-server)))

;;(setenv "FrameworkPathOverride" framework-path-override)

(provide 'joes-unity-wsl)
;;; joes-unity-wsl.el ends here
