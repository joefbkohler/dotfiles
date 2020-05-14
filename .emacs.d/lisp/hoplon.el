;;; package --- Custom Functions to work at Hoplon
;;; Commentary:
;;; Code:

(defun hoplon-fix-hmm-project ()
	"Fix all csproj files in unity.sln to work with Omnisharp."
	(interactive)
	(with-temp-buffer
		(insert-file-contents "/mnt/c/Devel/Work/leviathan/HMM-2017/unity/unity.sln")
		(while
			(search-forward "csproj" nil t)
			(let ((eow (point)))
				(when (search-backward "\"" (line-beginning-position) t)
					(forward-char)
					(let ((file-name (concat "/mnt/c/Devel/Work/leviathan/HMM-2017/unity/" (buffer-substring-no-properties (point) eow))))
						(with-temp-file file-name
							(progn
								(insert-file-contents file-name)
								(while
									(re-search-forward "c:" nil t)
									(replace-match "/mnt/c" t t))
								)))))
			(end-of-line)
			)))

(defun hoplon-client-log ()
 "Open server log."
  (interactive)
	(find-file "/mnt/c/Devel/Work/leviathan/HMM-2017/unity/Editor.log"))

(defun hoplon-server-log ()
  "Open client log."
  (interactive)
	(find-file "/mnt/c/Devel/Work/leviathan/HMM-2017/env/server/Editor.log"))

(defun hoplon-user-config ()
  "Open user.config."
  (interactive)
	(find-file "/mnt/c/Devel/Work/leviathan/user.config"))

(defun hoplon-omnisharp-load-hmm ()
	"Load HMM-2017-Repository to omnisharp."
	(interactive)
	(require 'omnisharp)
	(hoplon-fix-hmm-project)
	(setq omnisharp-server-executable-path "/mnt/c/Devel/omnisharp/stdio/run")
	(omnisharp--do-server-start "/mnt/c/Devel/Work/leviathan/HMM-2017/unity"))

(defun hoplon-omnisharp-load-customws ()
	"Load CustomWS to omnisharp."
	(interactive)
	(require 'omnisharp)
	(hoplon-fix-hmm-project)
	(setq omnisharp-server-executable-path "/mnt/c/Devel/omnisharp/stdio/run")
	(omnisharp--do-server-start "/mnt/c/Devel/Work/leviathan/HMM-2017/tools/"))

(add-hook
	'logview-mode-hook
	(lambda()
		(logview-choose-submode "Hoplon" "yyyy-MM-dd HH:mm:ss.SSS")))

(add-to-list 'auto-mode-alist '("EditorLog\\.txt$" . logview-mode))

(defun hoplon-downloaded-logs ()
	(interactive)
	(ido-find-file-in-dir "~/Downloads/logs")
	(logview-mode))

(defun hoplon-remote-server-logs ()
	(interactive)
	(let* (
			 (machine-dir (concat "//" (read-string "Machine name: ")))
			 (builds (directory-files (concat machine-dir "/deploy/HMM-2017/")))
			 (build-number (read-string "Build Number: "))
			 (build-dir ""))

		(mapc (lambda (build)
				  (when (string-match build-number build)
					  (setq build-dir build)))
			builds)
		(ido-find-file-in-dir (concat machine-dir "/deploy/HMM-2017/" build-dir "/logs"))))

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

;;(setq-default find-program "/bin/find")
;;(setq-default vc-hg-program "/ssh:joe.junior@10.0.2.2:/cygdrive/c/Program Files/TortoiseHg/hg.exe")
;;(setq-default omnisharp--curl-executable-path "/bin/curl")
;;(setq-default flycheck-csharp-omnisharp-curl-executable "/bin/curl")
;;(setq-default flycheck-csharp-omnisharp-codecheck-executable "/bin/curl")
;;(setq-default diff-command "/bin/diff")
;;(setenv "PATH" (concat "/bin:" (getenv "PATH")))

;; Fix so that vc-dir-find-file will find the file in the cygwin system
;;(defun around-vc-dir-find-file (orig-fun &rest args)
;; 	"Run cygpath in vc-dir-current-file to allow the windows HG output to work with cygwin."
;; 	(find-file (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat (concat "cygpath \"" (vc-dir-current-file)) "\""))))
;; 	)

;;(advice-add 'vc-dir-find-file :around #'around-vc-dir-find-file)


(provide 'hoplon)

;;; hoplon ends here
