(require 'joes-utils)

(defun my-exwm-hook(switch)
	(require 'exwm)
	(require 'exwm-config)
	(exwm-config-default)
	(exwm-init)
	(ido-mode 0)
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

(defun my-go-mode-hook ()
	(company-box-mode)
	(lsp)
	(local-set-key [remap indent-for-tab-command] 'indent-or-complete))

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
	(lsp-ui-doc-mode -1)
	(setq lsp-signature-auto-activate nil))

(defun my-csharp-mode-hook ()
	(require 'whitespace)

	(company-box-mode)
	(lsp)
	(lsp-ui-doc-mode 0)
	(yas-minor-mode)

	(setq c-default-style "linux" c-basic-offset 4)

	(setq company-idle-delay nil)

	(setq indent-tabs-mode nil)
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode 1)

	(c-set-offset 'inline-open 0)
	(c-set-offset 'func-decl-cont 0)

	;; Omnisharp Bindings
	(local-set-key [remap c-indent-line-or-region] 'indent-or-complete)
	(local-set-key (kbd "C-<tab>") 'company-complete)
	)

(provide 'hooks)
;;; hooks.el ends here
