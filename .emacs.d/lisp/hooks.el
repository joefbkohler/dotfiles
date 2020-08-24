(require 'joes-utils)

(defun my-exwm-hook(switch)
	(require 'exwm)
	(require 'exwm-config)
	(exwm-config-default)
	(exwm-init)
	(display-time))

(defun my-ediff-mode-hook()
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))

(defun my-vc-dir-mode-hook()
	(local-set-key (kbd "k") 'vc-dir-delete-marked-files)
	(local-set-key (kbd "r") 'vc-revert)
	(display-line-numbers-mode 0))

(defun my-pdf-view-mode-hook()
	(display-line-numbers-mode -1))

(defun my-latex-mode-hook ()
	(local-set-key [remap tex-compile] 'tex-compile-update)
	(lsp)
	(lsp-ui-mode t)
	(auto-fill-mode 1))

(defun my-typescript-mode-hook ()
	(require 'tide)
	(tide-setup)
	(tide-hl-identifier-mode 1)
	(eldoc-mode 1))

(defun my-python-mode-hook ()
	(add-hook 'before-save-hook 'delete-trailing-whitespace t)
	(company-box-mode)

	(highlight-indent-guides-mode t)
	(pyvenv-mode t)
	(lsp)
	(lsp-ui-mode t)
	(setq lsp-signature-auto-activate nil))

(defun my-csharp-mode-hook ()
	(require 'whitespace)
	;;(require 'ligature-mode)

	(company-box-mode)
	(lsp)
	(yas-minor-mode)

	(setq c-default-style "linux" c-basic-offset 4)

	(setq indent-tabs-mode nil)
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode 1)

	;;(ligature-mode 1)

	;;(set-buffer-file-coding-system 'dos)

	
	(c-set-offset 'inline-open 0)
	(c-set-offset 'func-decl-cont 0)

	;; Omnisharp Bindings
	(local-set-key [remap c-indent-line-or-region] 'indent-or-complete)
	(local-set-key (kbd "C-<tab>") 'company-complete)
	)

(provide 'hooks)
;;; hooks.el ends here
