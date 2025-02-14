
;;; Emacs --- Init file -*- lexical-binding: t; -*-
;;; Commentary:
;;; TODO:
;;; dependency checker: aspell/hunspell
;;; Modeline
;;; Check on Vertico.
;;; remove company? counsel completion-at-point the convolluted
;;; fix xref-apropos so it's useful. counsel, vertico, something!
;;; Use tree-sitter whenever possible
;;; --> Move native-comp-eln-cache to ~/.cache
;;; --> Move elpa to ~/.local/share
;;; --> Move backups, projects, recentf, tramp, transient, etc... to ~/.local/state

;;; Fix damn temp/backup files!

;;; Code:

;; Path
(add-to-list 'load-path "~/.config/emacs/joes")
(add-to-list 'exec-path "/usr/local/bin")

(require 'joes-utils)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'recentf)

;; Global variables
;; -- General
(setq-default user-full-name "Joe Köhler")
(setq-default user-mail-address "joe.fb.kohler@gmail.com")

(setq-default ring-bell-function 'joes-blink-minibuffer)
(setq-default scroll-conservatively 10000)
(setq-default scroll-step 1)
(setq-default tab-width 4)
(setq-default visible-bell nil)
(setq-default enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)

;; Local envinronment configuration
(ignore-errors (load-file "~/.config/emacs/local.el"))
(setq recentf-save-file "~/.config/emacs/recentf")
(setq custom-file "~/.config/emacs/emacs-custom.el")

;; Backup configuration
(make-directory "~/.config/emacs/autosaves/" t)
(setq backup-directory-alist `((".*" . "~/.config/emacs/autosaves/")))
(setq auto-save-file-name-transforms `((".*" "~/.config/emacs/autosaves/\\1" t)))

;; Default minor modes globally pre-loaded
(minibuffer-depth-indicate-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(cua-selection-mode 1)
(fringe-mode '(8 . 0))

(joes-keybindings-common)
(joes-theme-apply-default-faces)

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; -- External packages configuration and modes

(eval-when-compile
	(require 'use-package))

(setq use-package-compute-statistics t)

(use-package joes-package
	:demand t)

(use-package joes-prog
	:demand t
	:hook ((text-mode . joes-text-mode-hook)
		   (prog-mode . joes-prog-mode-hook)
		   (python-mode . joes-python-mode-hook)
		   (emacs-lisp-mode . joes-elisp-mode-hook)
		   (c-mode . joes-c-mode-hook)
		   (c-mode-common . joes-c-mode-common-hook)
		   (ediff-mode . joes-ediff-mode-hook)))

;; Useful to get the environment variables, only run in OSX
(use-package exec-path-from-shell
	:if (memq window-system '(mac ns))
	:config
	(declare-function exec-path-from-shell-initialize "exec-path-from-shell")
	(exec-path-from-shell-initialize))

(use-package zenburn-theme
	:config
	(joes-theme-apply-zenburn))

(use-package vterm
  :commands vterm vterm-other-window)

(use-package joes-ispell)

(use-package joes-mode-line)

(use-package joes-company
	:after joes-ispell)

(use-package joes-ivy
	:demand t
	:hook '(minibuffer-mode . joes-ivy-hook))

(use-package eglot
	:commands eglot-ensure
	:hook (eglot-managed-mode . joes-eglot-hook)
	:config
	(setq eglot-stay-out-of '(company)))

(use-package magit
	:commands magit-status
	:init
	(defun joes-git-commit-mode-hook ()
		(setq-local company-dabbrev-ignore-case nil)
		(setq-local company-dabbrev-downcase nil)
		(joes-keybindings-git-commit))
	:hook (git-commit-mode . joes-git-commit-mode-hook)
	:config
	(declare-function magit-auto-revert-mode "magit")
	(magit-auto-revert-mode -1))

(use-package flymake
	:config
	(setq-default elisp-flymake-byte-compile-load-path
		(append elisp-flymake-byte-compile-load-path load-path))
	(joes-theme-apply-flymake))

(use-package logview
	:mode ("\\.[lL][oO][gG]\\'$" . logview-mode)
	:config
	(joes-theme-apply-logview))

(use-package pdf-tools
	:mode ("\\.[pP][dD][fF]\\'$" . pdf-view-mode)
	:config
	(declare-function pdf-tools-install "pdf-tools")
	(pdf-tools-install :no-query))

(use-package joes-latex
  :hook (latex-mode . joes-latex-mode-hook))

(use-package joes-ai)

;; Terminal Emulator
(use-package eat
    :commands eat eat-other-window
    :config
    (declare-function eat-compile-terminfo "eat")
    (eat-compile-terminfo)
    (setq eat-kill-buffer-on-exit t))

(use-package ligature
	:functions global-ligature-mode
	:config
	(declare-function global-ligature-mode "ligature")
	(joes-theme-set-ligatures)
	(global-ligature-mode))

(provide 'init)
;;; init.el ends here.
