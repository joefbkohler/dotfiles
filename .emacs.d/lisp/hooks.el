(require 'joes-utils)
(require 'joes-keybindings)

(defun my-exwm-hook(switch)
	(require 'exwm)
	(require 'exwm-config)
	(set-exwm-keybindings)
	(exwm-config-default)
	(exwm-init)
	(set-common-exwm-simulation-keys)
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

(defun my-save-hook ()
	(delete-trailing-whitespace 0))

(defun my-go-mode-hook ()
	(lsp)
	(setq indent-tabs-mode nil))

(defun my-typescript-mode-hook ()
	(require 'tide)
	(tide-setup)
	(tide-hl-identifier-mode 1)
	(eldoc-mode 1))

(defun my-python-mode-hook ()
	(highlight-indent-guides-mode t)
	(pyvenv-mode t)
	(lsp)
	(lsp-ui-mode t)
	(lsp-ui-doc-mode -1)
	(setq lsp-signature-auto-activate nil))

(defun my-lsp-hook ()
	(set-lsp-keybinding))

(defun my-csharp-mode-hook ()
	(require 'whitespace)
	(csharp-tree-sitter-mode)
	(lsp)
	(yas-minor-mode)

	(setq indent-tabs-mode nil)
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode 1))

(provide 'hooks)
;;; hooks.el ends here
