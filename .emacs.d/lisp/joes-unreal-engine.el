;;; package --- Joe's Unreal Development functions
;;; Commentary:
;; Functions and configurations for Unreal Engine development.
;;; Code:

(require 'joes-utils)

(defun joes-unreal-compile-editor-project()
	"Find the Unreal project folder and run `make win` and `make linux'."
	(interactive)
	(make-local-variable compilation-buffer-name-function)
	(let ((project-file (string-trim (joes-search-file-regex-upward "*.uproject" 10)))
			 (compilation-buffer-name-function (lambda (mode)
												   "*compilation Unreal Windows*")))
		
		(compile (concat "cd " (file-name-directory project-file) " && make win"))
		
		(let ((compilation-buffer-name-function (lambda (mode)
													"*compilation Unreal*")))
			(compile (concat "cd " (file-name-directory project-file) " && make linux")))))

(provide 'joes-unreal-engine)
;;; joes-unreal-engine.el ends here
