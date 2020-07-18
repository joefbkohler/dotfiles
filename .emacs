;;; Emacs --- Init file
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Package
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
	'(package-selected-packages
		 (quote
			 (pdf-tools lsp-latex vue-mode lsp-ui jedi highlight-indent-guides pyvenv yaml-mode json-mode exec-path-from-shell dockerfile-mode tide typescript-mode eglot company-lsp lsp-mode jupyter gnu-elpa-keyring-update ivy exwm smartparens adaptive-wrap zenburn-theme smex logview ido-vertical-mode company flycheck))))

;; Check if all necessary packages are installed
;; if not, tries to install them
(catch 'pkg
	(dolist (pkg package-selected-packages)
		(unless (package-installed-p pkg)
			(message "Necessary packages not installed, intalling...")
			(package-refresh-contents)
			(package-install-selected-packages)
			(throw 'pkg pkg))))

;; Finished package configuration

(require 'joes-utils)
(require 'hooks)

;; Global variables
;; -- General
(add-to-list 'command-switch-alist '("-exwm" . my-exwm-hook))
(setq-default tab-width 4)

(setq visible-bell nil
	ring-bell-function (lambda ()
						   (invert-face 'mode-line)
						   (run-with-timer 0.1 nil #'invert-face 'mode-line)))

(setq-default visible-bell nil)
(setq-default scroll-step 1)
(setq-default scroll-conservatively 10000)
(setq-default visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default adaptive-wrap-extra-indent 1)
(setq-default python-shell-interpreter "/usr/bin/python3")
(setq-default display-line-numbers-grow-only 1)
(defvaralias 'lisp-indent-offset 'tab-width)
(add-to-list 'exec-path "/usr/local/bin")

;; -- Flycheck
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-navigation-minimum-level 'error)
(setq-default flycheck-check-syntax-automatically '(save idle-change mode-enabled))
(setq-default flycheck-idle-change-delay 5)
;; -- Ido
(setq-default ido-enable-regexp 1)
;; -- Company
(setq-default company-idle-delay 0)
(setq-default company-dabbrev-downcase nil)
(setq-default company-tooltip-align-annotations t)
;; -- LSP
(setq-default lsp-signature-auto-activate nil)

(put 'narrow-to-region 'disabled nil)

;; Minor modes globally pre-loaded
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
(cua-selection-mode 1)
;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; Major modes
(add-to-list 'auto-mode-alist '("\\.log$" . logview-mode))
(add-to-list 'auto-mode-alist '("\\.pdf$" . my-pdf-view-mode-hook))
(add-hook 'ediff-mode-hook 'my-ediff-mode-hook)
(add-hook 'vc-dir-mode-hook 'my-vc-dir-mode-hook)
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)
(add-hook 'typescript-mode-hook 'my-typescript-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'latex-mode-hook 'my-latex-mode-hook)

;; Custom key bindings
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-v"))

;; Linux only keybindings to simulate command of osx
(if (eq system-type 'gnu/linux)
	(progn
		(global-set-key (kbd "s-v") 'yank)
		(global-set-key (kbd "s-x") 'kill-region)
		(global-set-key (kbd "s-c") 'kill-ring-save)
		(global-set-key (kbd "s-u") 'revert-buffer)))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-|") 'toggle-window-split)
(global-set-key [remap shell-command] 'async-shell-command)
(define-key visual-line-mode-map [remap kill-line] nil) ;; Remove remapping of kill-line to kill-visual-line
(global-set-key [remap ido-toggle-prefix] 'ido-prev-match)
(global-set-key [remap previous-history-element] 'ido-prev-match)
(global-set-key [remap next-history-element] 'ido-next-match)
(global-set-key [remap ido-next-work-directory] 'ido-next-match)
(global-set-key [remap ido-prev-work-directory] 'ido-prev-match)
(global-set-key [remap ido-complete-space] 'ido-restrict-to-matches)
(global-set-key [remap isearch-forward-regexp] 'occur)
(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
(global-set-key [remap isearch-backward] 'isearch-backward-regexp)
(global-set-key [remap completion-at-point] 'company-complete)

;; system configuration
(setq backup-directory-alist
	`((".*" . "~/backups")))
(setq auto-save-file-name-transforms
	`((".*" "~/backups" t)))

;; Theme
(load-theme 'zenburn t)

(add-to-list 'default-frame-alist '(mouse-color . "#cca"))
(add-to-list 'default-frame-alist '(cursor-color . "#cca"))

(set-face-attribute 'default nil :background "#181818" :height 160 :font "-UKWN-Victor Mono-semibold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -1 :color "#555"))
(set-face-attribute 'region nil :foreground 'unspecified :background "#334")
(set-face-attribute 'font-lock-constant-face nil :foreground "#F0DFAF" :weight 'bold)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#A5A5A5" :weight 'bold)
(set-face-attribute 'font-lock-string-face nil :italic t)

(set-face-attribute 'flycheck-error nil :underline '(:color "#F00" :style wave))
(set-face-attribute 'company-tooltip-annotation nil :background 'unspecified)

(require 'logview)
(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified)


;; End Theme

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
