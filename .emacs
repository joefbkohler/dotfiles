;;; Emacs --- Init file
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(package-initialize)

(require 'joes-utils)
(require 'hoplon)

(require 'flycheck)
(require 'company)

;; Global variables
(setq-default tab-width 4)
(setq-default visible-bell 1)
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(setq-default flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq-default ido-enable-regexp 1)
(setq-default company-idle-delay 0)
(defvaralias 'lisp-indent-offset 'tab-width)

;; Minor modes pre-loaded
(cua-mode 1)
(global-linum-mode 1)
(ido-mode 1)
(ido-vertical-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
;;(yas-global-mode 1)
(global-flycheck-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(global-company-mode 1)
(ignore-errors (tool-bar-mode 0))
(menu-bar-mode 0)
(rainbow-delimiters-mode 1)

;; Major modes
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
 
(setq-default)
 
;; C# and Omnisharp Options
(add-hook
	'csharp-mode-hook
	(lambda ()
		(require 'company)
		(require 'omnisharp)
		(company-mode 1)
		(omnisharp-mode 1)
		
		(setq c-default-style "linux"
			c-basic-offset 4)
		
		(c-set-offset 'inline-open 0)
		(c-set-offset 'func-decl-cont 0)
 
		(make-local-variable 'company-idle-delay)
		(setq omnisharp-company-sort-results nil)
		(setq company-idle-delay nil)
		(add-to-list 'company-backends 'company-omnisharp)
 
		(flycheck-add-mode 'csharp-omnisharp-codecheck 'csharp-mode)
 
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
	)
 
;; Custom key bindings
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "M-x") 'smex)
;; This should go when emacs is updated!
(global-set-key (kbd "M-*") 'pop-tag-mark)
 
;; system configuration
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 
;; Theme
(load-theme 'darkburn t)
 
(set-face-attribute 'default nil :background "#181818")
(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -2 :color "#333"))
(set-cursor-color "#cca")
(set-face-attribute 'region nil :foreground 'unspecified :background "#334")
(set-face-attribute 'font-lock-constant-face nil :foreground "#F0DFAF" :weight 'bold)
 
(set-face-attribute 'flycheck-error nil :underline '(:color "#F00" :style wave))
(set-face-attribute 'company-tooltip-annotation nil :background 'unspecified)
(set-face-attribute 'default nil :font "-Bits-Bitstream Vera Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(set-frame-font "-Bits-Bitstream Vera Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1" nil t)
;; End Theme
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(package-selected-packages
		 (quote
			 (rainbow-delimiters multiple-cursors omnisharp adaptive-wrap darkburn-theme smex logview ido-vertical-mode company))))
 
(provide '.emacs)
;;; .emacs ends here
