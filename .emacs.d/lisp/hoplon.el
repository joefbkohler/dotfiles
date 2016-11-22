;;; package --- Custom Functions to work at Hoplon
;;; Commentary:
;;; Code:
(defun server-log ()
 "Open server log."
  (interactive)
  (find-file "/cygdrive/c/Devel/Work/leviathan/HMM/unity/EditorLog.txt")
  (logview-mode)
  )

(defun client-log ()
  "Open client log."
  (interactive)
  (find-file "/cygdrive/c/Devel/Work/leviathan/HMM/env/server/EditorLog.txt")
  (logview-mode)
	)

(defun logview-omnisharp-navigate-to-name()
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

(setq logview-additional-level-mappings
		 (quote
			 (("Hoplon"
				  (error "ERROR")
				  (warning "WARNING")
				  (information "INFO")
				  (debug "DEBUG")
				  (trace "STATS")))))

(setq logview-additional-submodes
		 (quote
			 (("Hoplon"
				  (format . "[TIMESTAMP] - [LEVEL] {NAME}")
				  (levels . "Hoplon")
				  (timestamp)))))

(setq-default find-program "/bin/find")

;; Fix so that vc-dir-find-file will find the file in the cygwin system
(defun around-vc-dir-find-file (orig-fun &rest args)
	"Runs cygpath in vc-dir-current-file to allow the windows HG output to work with cygwin."
	(find-file (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat (concat "cygpath \"" (vc-dir-current-file)) "\""))))
	)

(advice-add 'vc-dir-find-file :around #'around-vc-dir-find-file)


(provide 'hoplon)

;;; hoplon ends here
