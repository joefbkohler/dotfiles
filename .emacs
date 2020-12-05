;;; Emacs --- Init file
;;; Commentary:
;;; Code:

(add-to-list 'command-switch-alist '("-exwm" . my-exwm-hook))

;; Path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'joes-utils)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'hooks)

;; -- Package configuration
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f56eb33cd9f1e49c5df0080a3e8a292e83890a61a89bceeaa481a5f183e8e3ef" default))
 '(ediff-split-window-function 'split-window-horizontally)
 '(package-selected-packages
   '(smex doom-modeline lsp-ivy lsp-treemacs counsel magit-lfs company-box csharp-mode magit yasnippet pdf-tools lsp-latex vue-mode lsp-ui jedi highlight-indent-guides pyvenv yaml-mode json-mode exec-path-from-shell dockerfile-mode typescript-mode eglot lsp-mode jupyter gnu-elpa-keyring-update ivy exwm smartparens adaptive-wrap zenburn-theme logview company flycheck)))
;; Finished package configuration

;; -- Keybindings
(when (eq system-type 'gnu/linux)
	(simulate-command-key))

(set-common-keybindings)

;; Global variables
;; -- General
(ignore-error (defvaralias 'lisp-indent-offset 'tab-width))
(setq-default display-line-numbers-grow-only 1)
(setq-default python-shell-interpreter "/usr/bin/python3")
(setq-default ring-bell-function 'blink-minibuffer)
(setq-default scroll-conservatively 10000)
(setq-default scroll-step 1)
(setq-default tab-width 4)
(setq-default visible-bell nil)
(setq-default visual-line-fringe-indicators 'left-curly-arrow right-curly-arrow)
(put 'narrow-to-region 'disabled nil)
;; Backup configuration
(setq backup-directory-alist `((".*" . "~/backups")))
(setq auto-save-file-name-transforms `((".*" "~/backups" t)))
(setq-default lsp-signature-auto-activate nil)

;; Major modes configuration
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-tools-install))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(add-hook 'pdf-view-mode-hook 'my-pdf-view-mode-hook)
(add-hook 'ediff-mode-hook 'my-ediff-mode-hook)
(add-hook 'vc-dir-mode-hook 'my-vc-dir-mode-hook)
(add-hook 'typescript-mode-hook 'my-typescript-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'latex-mode-hook 'my-latex-mode-hook)

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; Default minor modes globally pre-loaded
(column-number-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)
(cua-selection-mode 1)

;; -- External packages configuration and modes

(defvar-local initialization-errors "")

;; -- Zenburn
(condition-case err
	(apply-zenburn-theme)
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Adaptive wrap
(condition-case err
	(adaptive-wrap-prefix-mode 1)
	(global-adaptive-wrap-prefix-mode 1)
	(setq-default adaptive-wrap-extra-indent 1)
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Ivy configuration
(condition-case err
	(progn
	  (require 'counsel)
	  (require 'smex)
	  (ivy-mode 1)
	  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
	  (set-ivy-keybindings))
  (error
   (setq-local initialization-errors (error-message-string err))))

;; -- Company configuration
(condition-case err
	(progn
		(setq-default company-idle-delay 0)
		(setq-default company-dabbrev-downcase nil)
		(setq-default company-tooltip-align-annotations t)
		(global-company-mode 1)
		(apply-company-theme)
		(set-company-keybindings))
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- FLycheck configuration
(condition-case err
	(progn
		(setq-default flycheck-emacs-lisp-load-path 'inherit)
		(setq-default flycheck-navigation-minimum-level 'error)
		(setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled))
		(setq-default flycheck-idle-change-delay 5)
		(global-flycheck-mode 1)
		(apply-flycheck-theme))
	(error
		(setq-local initialization-errors (error-message-string err))))

(when (not (= (length initialization-errors) 0))
	(error "%s" "Some error occurred during initialization."))

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
