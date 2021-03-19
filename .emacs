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

(require 'joes-packages-manager)
(my-straight-initialize)
(setq-default package-enable-at-startup nil)
(setq-default straight-vc-git-default-protocol 'ssh)
(my-register-fork-packages)
(my-install-default-packages)
;;; Finished package configuration

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
(setq-default ispell-complete-word-dict "/home/joe/.dict/words")
(put 'narrow-to-region 'disabled nil)
;; Global hooks
(add-hook 'before-save-hook 'my-save-hook)
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
		(setq-default lsp-completion-show-detail nil)
		(setq-default lsp-completion-show-kind nil)
		(setq-default lsp-ui-doc-enable nil)
		(setq-default lsp-ui-sideline-enable nil)
		(add-hook 'lsp-after-open-hook 'my-lsp-hook))
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
		(add-to-list 'ivy-initial-inputs-alist '(counsel-describe-variable . ""))
		(add-to-list 'ivy-initial-inputs-alist '(counsel-describe-function . ""))
		(setq-default ivy-use-virtual-buffers t)
		(setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
		(setq-default xref-show-definitions-function #'ivy-xref-show-defs)
		(setq-default xref-show-xrefs-function #'ivy-xref-show-xrefs)

		(ivy-configure 'ivy-switch-buffer :display-transformer-fn 'ivy-switch-buffer-mode-path-transformer)
		(ivy-configure 'counsel-M-x :display-transformer-fn 'ivy-counsel-function-doc-transformer)
		(ivy-configure 'counsel-describe-variable :display-transformer-fn 'ivy-counsel-variable-doc-transformer)
		(ivy-configure 'counsel-describe-function :display-transformer-fn 'ivy-counsel-function-doc-transformer)

		(set-ivy-keybindings))
	(error
		(setq-local initialization-errors (error-message-string err))))

;; -- Company configuration
;;(condition-case err
;;    (progn
;;      (require 'company)
;;      (require 'company-box)
;; 		(setq-default company-idle-delay nil)
;; 		(setq-default company-dabbrev-downcase nil)
;; 		(setq-default company-tooltip-align-annotations t)
;; 		(setq-default company-tooltip-minimum-width 70)
;; 		(setq-default company-box-doc-delay 0)
;; 		(setq-default company-tooltip-maximum-width 70)
;; 		(add-hook 'company-mode-hook 'my-company-hook)
;; 		(global-company-mode 1)
;; 		(apply-company-theme)
;; 		(set-company-keybindings))
;; 	(error
;; 		(setq-local initialization-errors (error-message-string err))))

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

;; -- Tree-Sitter configuration
(condition-case err
	(progn
		(require 'tree-sitter)
		(require 'tree-sitter-langs)
		(global-tree-sitter-mode)
		(apply-tree-sitter-theme)
		)
	(error
		(setq-local initialization-errors (error-message-string err))))


(when (not (= (length initialization-errors) 0))
	(error "%s \n\n error: %s" "Some error occurred during initialization.'" initialization-errors))

(provide '.emacs)

;; TODO:
;; Modeline
;; Try to fix lsp Ivy workspace Symbol
;; Try to fix company-box when too big signature
;; Yasnippet

;;; .emacs ends here
