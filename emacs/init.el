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
;; --> Move elpa to ~/.local/share

;;; Code:

;; Global variables
;; -- General
(setq-default user-full-name "Joe Köhler")
(setq-default user-mail-address "joe.fb.kohler@gmail.com")

(setq-default ring-bell-function 'joes-blink-minibuffer)
(setq-default scroll-conservatively 10000)
(setq-default pixel-scroll-precision-mode t)
(setq-default scroll-step 1)
(setq-default tab-width 4)
(setq-default visible-bell nil)
(setq-default enable-recursive-minibuffers t)
(put 'narrow-to-region 'disabled nil)

;; Local envinronment configuration
(ignore-errors (load-file (expand-file-name "emacs/local.el" (xdg-config-home))))

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

;; Minor modes globally unloaded
(menu-bar-mode 0)
(ignore-errors (scroll-bar-mode 0))
(ignore-errors (tool-bar-mode 0))

;; -- External packages configuration and modes

(eval-when-compile
	(require 'use-package))

(setq use-package-compute-statistics t)

(require 'joes-utils)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'joes-package)

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
        (joes-darker-transparent-background)))
  
(use-package joes-ispell)
 
(use-package joes-mode-line
    :demand t
    :hook ((after-save . joes-update-mode-line-vc)
              (find-file . joes-update-mode-line-vc)
              (window-configuration-change . joes-update-mode-line-vc)))
 
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
