(require 'joes-utils)

(defun my-exwm-hook(switch)
	(require 'exwm)
	(require 'exwm-config)
	(exwm-config-default)
	(exwm-init)
	(display-time))

(defun my-ediff-mode-hook()
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally))
	)

(defun my-vc-dir-mode-hook()
	(local-set-key (kbd "k") 'vc-dir-delete-marked-files)
	(local-set-key (kbd "r") 'vc-revert)
	(display-line-numbers-mode 0))

(defun my-csharp-mode-hook ()
	(require 'company)
	(require 'omnisharp)
	(require 'whitespace)
	
	(omnisharp-mode 1)

	(setq c-default-style "linux" c-basic-offset 4)

	(setq indent-tabs-mode nil)
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark lines))
	(setq whitespace-line-column 100)
	(set-face-attribute 'whitespace-line nil :underline '(:color "#990") :background nil :foreground nil)
	(whitespace-mode 1)
							   
	;;(set-buffer-file-coding-system 'dos)
	
	(c-set-offset 'inline-open 0)
	(c-set-offset 'func-decl-cont 0)
	
	(make-local-variable 'company-idle-delay)
	;;(setq omnisharp-company-sort-results nil)
	(setq company-idle-delay nil)
	(add-to-list 'company-backends 'company-omnisharp)
	
	(add-hook 'compilation-mode-hook 'omnisharp-find-usages-visuals)
	
	;; Omnisharp Bindings
	(local-set-key [remap c-indent-line-or-region] 'indent-or-complete)
	(local-set-key (kbd "C-<tab>") 'company-complete)
	(local-set-key (kbd "{") 'c-electric-brace)
	(local-set-key (kbd "C-c f") 'omnisharp-navigate-to-solution-file)
	(local-set-key (kbd "C-c t") 'omnisharp-navigate-to-solution-type)
	(local-set-key (kbd "C-c d") 'omnisharp-go-to-definition)
	(local-set-key (kbd "C-c u") 'omnisharp-find-usages)
	(local-set-key (kbd "C-c i") 'omnisharp-find-implementations)
	(local-set-key (kbd "C-c m") 'omnisharp-navigate-to-current-file-member)
	(local-set-key (kbd "C-c M-m") 'omnisharp-navigate-to-solution-member)
	)

(provide 'hooks)
;;; hooks.el ends here
