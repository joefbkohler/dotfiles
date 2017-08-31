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
(require 'adaptive-wrap)
(require 'logview)

;; Global variables
;; -- General
(setq-default tab-width 4)
(setq-default visible-bell 1)
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default adaptive-wrap-extra-indent 1)
(setq-default python-shell-interpreter "/usr/bin/python3")
(defvaralias 'lisp-indent-offset 'tab-width)

;; -- Flycheck
(setq-default flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
;; -- Ido
(setq-default ido-enable-regexp 1)
;; -- Company
(setq-default company-idle-delay 0)
(setq-default company-dabbrev-downcase nil)

(put 'narrow-to-region 'disabled nil)

;; Minor modes globally pre-loaded
(cua-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(global-adaptive-wrap-prefix-mode 1)
(ido-mode 1)
(ido-vertical-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
;;(yas-global-mode 1)
(global-flycheck-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(global-company-mode 1)

;; Minor modes globally unloaded
(menu-bar-mode 0)
(scroll-bar-mode 0)
(ignore-errors (tool-bar-mode 0))

;; Major modes
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
 
(add-hook
	'ediff-mode-hook
	(lambda()
		(setq ediff-split-window-function 'split-window-horizontally)
		))

(add-hook
	'vc-dir-mode-hook
	(lambda()
		(local-set-key (kbd "k") 'vc-dir-delete-marked-files)
		(local-set-key (kbd "r") 'vc-revert)
		(linum-mode 0)))

(add-hook
	'logview-mode-hook
	(lambda()
		(linum-mode 0)))

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
	)

;; Custom key bindings
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-\\") 'toggle-window-split)
(global-set-key [remap previous-history-element] 'ido-prev-match)
(global-set-key [remap next-history-element] 'ido-next-match)
(global-set-key [remap ido-next-work-directory] 'ido-next-match)
(global-set-key [remap ido-prev-work-directory] 'ido-prev-match)
(global-set-key [remap ido-complete-space] 'ido-restrict-to-matches)
(global-set-key [remap isearch-forward-regexp] 'occur)
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)
(global-set-key (kbd "M-*") 'pop-tag-mark) ;; This should go when emacs is updated!
 
;; system configuration
(setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 
;; Theme
(load-theme 'darkburn t)

(set-cursor-color "#cca")
(set-mouse-color "#cca")
(set-frame-font "-CYEL-Iosevka Term-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1" nil t)

(set-face-attribute 'linum nil :height 130)
(set-face-attribute 'default nil :background "#181818" :height 130)
(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -2 :color "#333"))
(set-face-attribute 'region nil :foreground 'unspecified :background "#334")
(set-face-attribute 'font-lock-constant-face nil :foreground "#F0DFAF" :weight 'bold)
 
(set-face-attribute 'flycheck-error nil :underline '(:color "#F00" :style wave))
(set-face-attribute 'company-tooltip-annotation nil :background 'unspecified)

(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified)


;; End Theme
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(package-selected-packages
		 (quote
			 (smartparens request-deferred omnisharp expand-region multiple-cursors adaptive-wrap darkburn-theme smex logview ido-vertical-mode company))))
 
(provide '.emacs)
;;; .emacs ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
