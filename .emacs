;;; Emacs --- Init file
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(package-initialize)

(require 'joes-utils)
(require 'hoplon)
(require 'hooks)

(require 'flycheck)
(require 'company)
(require 'adaptive-wrap)
(require 'logview)

;; Global variables
;; -- General
(add-to-list 'command-switch-alist '("-exwm" . my-exwm-hook))
(setq-default tab-width 4)
(setq-default visible-bell 1)
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default adaptive-wrap-extra-indent 1)
(setq-default python-shell-interpreter "/usr/bin/python3")
(setq-default display-line-numbers-grow-only 1)
(defvaralias 'lisp-indent-offset 'tab-width)

;; -- Flycheck
(setq-default flycheck-check-syntax-automatically '(save mode-enabled new-line))
(setq-default flycheck-emacs-lisp-load-path 'inherit)
;; -- Ido
(setq-default ido-enable-regexp 1)
;; -- Company
(setq-default company-idle-delay 0)
(setq-default company-dabbrev-downcase nil)

(put 'narrow-to-region 'disabled nil)

;; Minor modes globally pre-loaded
(cua-mode 1)
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
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; Major modes
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
(add-hook 'ediff-mode-hook 'my-ediff-mode-hook)
(add-hook 'vc-dir-mode-hook 'my-vc-dir-mode-hook)
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Theme
(load-theme 'zenburn t)

(add-to-list 'default-frame-alist '(mouse-color . "#cca"))
(add-to-list 'default-frame-alist '(cursor-color . "#cca"))

(set-face-attribute 'default nil :background "#181818" :height 150 :font "-CYEL-Iosevka Term-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -1 :color "#333"))
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
			 (gnu-elpa-keyring-update avy ivy exwm smartparens request-deferred omnisharp expand-region adaptive-wrap zenburn-theme smex logview ido-vertical-mode company))))

(provide '.emacs)
;;; .emacs ends here

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
