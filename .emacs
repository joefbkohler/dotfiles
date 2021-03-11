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
		 '(ivy-xref tree-sitter go-mode smex lsp-ivy counsel magit-lfs company-box csharp-mode magit yasnippet pdf-tools lsp-latex vue-mode lsp-ui jedi highlight-indent-guides pyvenv yaml-mode json-mode dockerfile-mode typescript-mode eglot lsp-mode jupyter gnu-elpa-keyring-update ivy exwm smartparens adaptive-wrap zenburn-theme logview company flycheck)))
;; Finished package configuration

;; -- Keybindings
(when (not (eq system-type 'darwin))
	(simulate-command-key))

(set-common-keybindings)

;; Global variables
;; -- General
(setq-default display-line-numbers-grow-only 1)
(setq-default python-shell-interpreter "/usr/bin/python3")
(setq-default ring-bell-function 'blink-minibuffer)
(setq-default scroll-conservatively 10000)
(setq-default scroll-step 1)
(setq-default tab-width 4)
(setq-default lisp-indent-offset 4)
(setq-default visible-bell nil)
(setq-default visual-line-fringe-indicators 'left-curly-arrow right-curly-arrow)
(setq-default project-file-extensions (delete-dups (append project-file-extensions '("cs" "go" "py" "tex"))))
(put 'narrow-to-region 'disabled nil)
;; Backup configuration
(setq backup-directory-alist `((".*" . "~/backups")))
(setq auto-save-file-name-transforms `((".*" "~/backups" t)))

;; Local envinronment configuration
(ignore-errors (load-file "~/.emacs-local"))

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
(add-hook 'go-mode-hook 'my-go-mode-hook)

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

;; -- LSP
(condition-case err
	(progn
		(require 'lsp)
		(require 'ivy)
		(setq-default lsp-signature-auto-activate nil)
		(setq-default lsp-enable-file-watchers nil)
		(setq-default lsp-ui-doc-enable nil)
		(setq-default lsp-ui-sideline-enable nil)
		(add-hook 'lsp-mode-hook 'my-lsp-hook))
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Zenburn
(condition-case err
	(apply-zenburn-theme)
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Adaptive wrap
(condition-case err
	(require 'adaptive-wrap)
	(adaptive-wrap-prefix-mode 1)
	(global-adaptive-wrap-prefix-mode 1)
	(setq-default adaptive-wrap-extra-indent 1)
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Ivy configuration
(condition-case err
	(progn
		(require 'ivy)
		(require 'counsel)
		(require 'ivy-xref)
		(ivy-mode 1)
		(add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . ""))
		(setq-default ivy-use-virtual-buffers t)
		(setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
		(setq-default xref-show-definitions-function #'ivy-xref-show-defs)
		(setq-default xref-show-xrefs-function #'ivy-xref-show-xrefs)

		(ivy-configure 'ivy-switch-buffer :display-transformer-fn 'ivy-switch-buffer-mode-path-transformer)
		(ivy-configure 'counsel-M-x :display-transformer-fn 'ivy-counsel-mx-doc-transformer)
		
		(set-ivy-keybindings))
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Company configuration
(condition-case err
    (progn
      (require 'company)
      (require 'company-box)
		(setq-default company-idle-delay nil)
		(setq-default company-dabbrev-downcase nil)
		(setq-default company-tooltip-align-annotations t)
		(setq-default company-tooltip-minimum-width 70)
		(setq-default company-box-doc-delay 0)
		(setq-default company-tooltip-maximum-width 70)
		(add-hook 'company-mode-hook 'my-company-hook)
		(global-company-mode 1)
		(apply-company-theme)
		(set-company-keybindings))
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- FLycheck configuration
(condition-case err
	(progn
		(require 'flycheck)
		(setq-default flycheck-emacs-lisp-load-path 'inherit)
		(setq-default flycheck-navigation-minimum-level 'error)
		(setq-default flycheck-check-syntax-automatically '(save new-line idle-buffer-switch mode-enabled))
		(global-flycheck-mode 1)
		(apply-flycheck-theme))
	(error
		(setq-local initialization-errors (error-message-string err))))

(when (not (= (length initialization-errors) 0))
	(error "%s \n\n error: %s" "Some error occurred during initialization. Try running: `M-x package-refresh-contents' then `M-x package-install-selected-packages'" initialization-errors))

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO:
;; Modeline
;; Try to fix lsp Ivy workspace Symbol
;; Try to fix company-box when too big signature
;; Yasnippet

;; tree sitter
;; (require 'tree-sitter)
;; (require 'tree-sitter-langs)
;; (global-tree-sitter-mode)
;; tree-sitter colors: (tree-sitter-hl-add-patterns 'c-sharp [(name_colon (identifier)* @label)])
