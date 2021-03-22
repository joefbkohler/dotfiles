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
	(fringe-mode '(8 . 0))
	(display-time))

(defun my-ediff-mode-hook()
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))

(defun my-prog-mode-hook ()
	(flymake-mode 1)
	(if (< (count-lines (point-min) (point-max))
			  5000)
		(linum-mode 1)
		(display-line-numbers-mode 1)))

(defun my-vc-dir-mode-hook()
	(local-set-key (kbd "k") 'vc-dir-delete-marked-files)
	(local-set-key (kbd "r") 'vc-revert))

(defun my-latex-mode-hook ()
	(local-set-key [remap tex-compile] 'tex-compile-update)
	(lsp)
	(flymake-mode 1)
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
	(push 'indent-or-complete python-indent-trigger-commands)
	(lsp))

(defun my-lsp-hook ()
	(yas-minor-mode)
	(set-lsp-keybinding))

(defun my-csharp-mode-hook ()
	(require 'whitespace)
	(csharp-tree-sitter-mode)
	(lsp)

	(setq indent-tabs-mode nil)
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode 1))

(provide 'hooks)
;;; hooks.el ends here
