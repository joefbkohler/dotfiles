;;; Emacs --- Init file -*- lexical-binding: t; -*-
;; Copyright (C) 2025  Joe Köhler

;; Author: Joe Köhler <joe.fb.kohler@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:

;; TODO:
;; dependency checker: aspell/hunspell
;; Check on Vertico.
;; remove company? counsel completion-at-point the convolluted
;; fix xref-apropos so it's useful.  counsel, vertico, something!
;; Use tree-sitter whenever possible

;;; Code:
(require 'joes-utils)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'joes-package)
(require 'xdg)

;; Global variables
;; -- General
(setopt
    user-full-name  "Joe Köhler"
    user-mail-address "joe.fb.kohler@gmail.com"

    ring-bell-function 'joes-blink-minibuffer
    scroll-conservatively 10000
    pixel-scroll-precision-mode t
    scroll-step 1
    visible-bell nil
    enable-recursive-minibuffers t
    compilation-ask-about-save nil
    compilation-save-buffers-predicate 'ignore
    grep-save-buffers nil)

(put 'narrow-to-region 'disabled nil)
(setq-default tab-width 4)

;; Local envinronment configuration
(ignore-errors (load-file (expand-file-name "emacs/local.el" (xdg-config-home))))

;; Default minor modes globally pre-loaded
(minibuffer-depth-indicate-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(delete-selection-mode 1)
(cua-selection-mode 1)
(fringe-mode '(8 . 0))

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; -- External packages configuration and modes

(joes-package-initialize)

(eval-when-compile
	(require 'use-package))

(setq use-package-compute-statistics t)

(use-package joes-prog
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
    (joes-theme-apply-zenburn)
    (when joes-use-transparency
        (joes-theme-darker-transparent-background)))
  
(use-package joes-ispell)
 
(use-package joes-mode-line
	:demand t
	:hook ((after-save . joes-mode-line-update-vc)
			  (find-file . joes-mode-line-update-vc)
			  (window-configuration-change . joes-mode-line-update-vc))
    :config
    (add-hook 'window-selection-change-functions 'joes-mode-line-update-window))
  
(use-package eglot
    :commands eglot-ensure
    :hook (eglot-managed-mode . joes-eglot-hook))
 
(use-package cape
    :config
    (declare-function cape-capf-super "cape")
    (declare-function cape-dabbrev "cape")
    (declare-function cape-file "cape")
    (declare-function cape-wrap-inside-code "cape")
    (add-hook 'completion-at-point-functions (cape-capf-super #'cape-dabbrev #'ispell-completion-at-point))
    (add-hook 'completion-at-point-functions #'cape-file)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-inside-code))

(use-package magit
    :commands magit-status
    :init
    (defun joes-git-commit-mode-hook ()
    	(setq-local company-dabbrev-ignore-case nil)
    	(setq-local company-dabbrev-downcase nil)
        (joes-keybindings-git-commit)
        (joes-theme-apply-magit))
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
    :init
    (joes-keybindings-eat)
    :config
    (declare-function eat-compile-terminfo "eat")
    (eat-compile-terminfo)
    (setopt eat-kill-buffer-on-exit t))
 
(use-package ligature
    :functions global-ligature-mode
    :config
    (declare-function global-ligature-mode "ligature")
    (joes-theme-set-ligatures)
    (global-ligature-mode))

(use-package vertico
    :config
    (declare-function vertico-mode "vertico")
    (declare-function vertico-multiform-mode "vertico")
    (vertico-mode)
    (vertico-multiform-mode)
    (joes-keybindings-vertico)
    (joes-theme-apply-vertico)
    (setopt vertico-multiform-categories
      '((file grid)
        (consult-grep buffer))))

(use-package marginalia
    :config
    (declare-function marginalia-mode "marginalia")
    (marginalia-mode))

(use-package orderless
    :config
    (setopt completion-styles '(basic orderless partial-completion)))

(use-package consult
    :demand t
    :init
    (defun joes-minibuffer-complete()
        "This function allows use of tab partial completion when in minibuffer.
It still allows consult completion in eval-expression."
        (when (not (string= current-minibuffer-command "eval-expression"))
            (setq-local completion-in-region-function #'completion--in-region)))
    :hook ((minibuffer-mode . joes-minibuffer-complete))
    :config
    (recentf-mode)
    (declare-function consult-completion-in-region "consult")
    (setq-default completion-in-region-function #'consult-completion-in-region)
    (joes-keybindings-consult))

(provide 'init)
;;; init.el ends here.
