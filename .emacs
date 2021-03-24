;;; Emacs --- Init file
;;; Commentary:
;;; TODO:
;;; Fix list of company-backends for different modes.
;;; Fix keep-prefix when completing dabbrev with counsel.
;;; Add a sane initialization to tree-sitter-indent
;;; Fix damn temp/backup files!
;;; Modeline
;;; Try to fix lsp Ivy workspace Symbol

;;; Code:

(add-to-list 'command-switch-alist '("-exwm" . my-exwm-hook))

;; Path
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "/usr/local/bin")

(require 'joes-utils)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'hooks)

;; -- Package (Straight)configuration
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
(setq-default lisp-indent-offset 4)
(setq-default tab-width 4)
(setq-default visible-bell nil)
(setq-default visual-line-fringe-indicators 'left-curly-arrow right-curly-arrow)
(setq-default project-file-extensions (delete-dups (append project-file-extensions '("cs" "go" "py" "tex"))))
(setq-default ispell-complete-word-dict "/home/joe/.dict/words")
(setq-default elisp-flymake-byte-compile-load-path (append elisp-flymake-byte-compile-load-path load-path))
(put 'narrow-to-region 'disabled nil)

;; Backup configuration
(setq backup-directory-alist `((".*" . "~/backups")))
(setq auto-save-file-name-transforms `((".*" "~/backups" t)))

;; Local envinronment configuration
(ignore-errors (load-file "~/.emacs-local"))

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; Global hooks
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'before-save-hook 'my-save-hook)

;; Default minor modes globally pre-loaded
(column-number-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(cua-selection-mode 1)
(fringe-mode '(8 . 0))

;; Major modes hooks
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-tools-install))
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(add-hook 'ediff-mode-hook 'my-ediff-mode-hook)
(add-hook 'vc-dir-mode-hook 'my-vc-dir-mode-hook)
(add-hook 'typescript-mode-hook 'my-typescript-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'latex-mode-hook 'my-latex-mode-hook)
(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; -- External packages configuration and modes

(defvar-local initialization-errors "")

;; -- LSP
(condition-case err
	(progn
		(setq-default lsp-signature-auto-activate nil)
		(setq-default lsp-enable-file-watchers nil)
		(setq-default lsp-enable-indentation nil)
		(add-hook 'lsp-after-open-hook 'my-lsp-hook))
	(error
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))

;; -- Magit
(condition-case err
	(magit-auto-revert-mode -1)
	(error
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))

;; -- Zenburn
(condition-case err
	(apply-zenburn-theme)
	(error
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))

;; -- Company
(condition-case err
	(progn
		(global-company-mode 1)
		(setq-default company-dabbrev-ignore-case 'keep-prefix)
		(setq-default company-idle-delay nil))
	(error
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))

;; -- Ivy configuration
(condition-case err
	(progn
		(require 'counsel)
		(require 'ivy-xref)
		(ivy-mode 1)
		(ivy-prescient-mode 1)
		(prescient-persist-mode 1)

		(setq-default ivy-initial-inputs-alist
			(append
				ivy-initial-inputs-alist
				'((counsel-M-x . "")
					 (counsel-describe-variable . "")
					 (counsel-describe-function . ""))))

		(setq-default ivy-prescient-sort-commands
			(append ivy-prescient-sort-commands '(lsp-ivy-workspace-symbol)))

		(setq-default ivy-use-virtual-buffers t)
		(setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
		(setq-default xref-show-definitions-function #'ivy-xref-show-defs)
		(setq-default xref-show-xrefs-function #'ivy-xref-show-xrefs)

		(ivy-configure 'ivy-switch-buffer :display-transformer-fn 'ivy-switch-buffer-mode-path-transformer)
		(ivy-configure 'counsel-M-x :display-transformer-fn 'ivy-counsel-function-doc-transformer)
		(ivy-configure 'counsel-describe-variable :display-transformer-fn 'ivy-counsel-variable-doc-transformer)
		(ivy-configure 'counsel-describe-function :display-transformer-fn 'ivy-counsel-function-doc-transformer)

		(apply-ivy-theme)
		(set-ivy-keybindings))
	(error
		(ido-everywhere 1)
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))

;; -- Tree-Sitter configuration
(condition-case err
	(progn
		(global-tree-sitter-mode)
		(add-hook 'tree-sitter-mode-hook 'my-tree-sitter-mode-hook)
		(apply-tree-sitter-theme))
	(error
		(setq-local initialization-errors (concat initialization-errors (error-message-string err) "\n"))))


(when (not (= (length initialization-errors) 0))
	(error "%s \n\n error: %s" "Some error occurred during initialization.'" initialization-errors))

(provide '.emacs)
;;; .emacs ends here
