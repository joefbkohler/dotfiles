;;; package --- Custom Functions to work at Hoplon
;;; Commentary:
;;; Code:
(defun hoplon-server-log ()
 "Open server log."
  (interactive)
	(find-file "/cygdrive/c/Devel/Work/leviathan/HMM/unity/Editor.log"))

(defun hoplon-client-log ()
  "Open client log."
  (interactive)
	(find-file "/cygdrive/c/Devel/Work/leviathan/HMM/env/server/Editor.log"))

(defun hoplon-user-config ()
  "Open user.config."
  (interactive)
	(find-file "/cygdrive/c/Devel/Work/leviathan/user.config"))

(add-hook
	'logview-mode-hook
	(lambda()
		(logview-choose-submode "Hoplon" "yyyy-MM-dd HH:mm:ss.SSS")))

(add-to-list 'auto-mode-alist '("EditorLog\\.txt$" . logview-mode))

(defun hoplon-logview-omnisharp-navigate-to-name()
	(interactive)
	(require 'omnisharp)
	(let ((name (nth 1 (split-string (car (split-string (thing-at-point 'line t) "}")) "{")))
			 (quick-fix (omnisharp-post-message-curl-as-json (concat (omnisharp-get-host) "findtypes"))))
		(catch 'filename
			(dolist (item (omnisharp--vector-to-list (cdr (assoc 'QuickFixes quick-fix))))
				(when (string-match name (cdr(assoc 'Text item)))
					(omnisharp--find-file-possibly-in-other-window (cdr (assoc 'FileName item)))
					(throw 'filename (cdr (assoc 'FileName item)))
					)
			))
		)
	)

(defun hoplon-downloaded-logs ()
	(interactive)
	(ido-find-file-in-dir "~/Downloads/logs")
	(logview-mode))

(defun hoplon-remote-server-logs ()
	(interactive)
	(let* (
			 (machine-dir (concat "//" (read-string "Machine name: ")))
			 (builds (directory-files (concat machine-dir "/deploy/HMM/")))
			 (build-number (read-string "Build Number: "))
			 (build-dir ""))
		
		(mapc (lambda (build)
				  (when (string-match build-number build)
					  (setq build-dir build)))
			builds)
		(ido-find-file-in-dir (concat machine-dir "/deploy/HMM/" build-dir "/logs"))))

(setq logview-additional-level-mappings
		 (quote
			 (("Hoplon"
				  (error "ERROR")
				  (warning "WARNING")
				  (information "INFO" "EVENT")
				  (debug "DEBUG")
				  (trace "STATS")))))

(setq logview-additional-submodes
		 (quote
			 (("Hoplon"
				  (format . "[TIMESTAMP] - [LEVEL] {NAME}")
				  (levels . "Hoplon")
				  (timestamp)))))
(add-hook
	'logview-mode-hook
	(lambda ()
		(local-set-key (kbd "j") 'logview-omnisharp-navigate-to-name)
		))

(setq-default find-program "/bin/find")
(setq-default vc-hg-program "hg.exe")
(setq-default omnisharp--curl-executable-path "/bin/curl")
(setq-default flycheck-csharp-omnisharp-curl-executable "/bin/curl")
(setq-default flycheck-csharp-omnisharp-codecheck-executable "/bin/curl")
(setq-default diff-command "/bin/diff")
(setenv "PATH" (concat "/bin:" (getenv "PATH")))

;; Fix so that vc-dir-find-file will find the file in the cygwin system
(defun around-vc-dir-find-file (orig-fun &rest args)
	"Run cygpath in vc-dir-current-file to allow the windows HG output to work with cygwin."
	(find-file (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat (concat "cygpath \"" (vc-dir-current-file)) "\""))))
	)

(advice-add 'vc-dir-find-file :around #'around-vc-dir-find-file)


(provide 'hoplon)

;;; hoplon ends here
