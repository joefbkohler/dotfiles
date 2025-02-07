;;; joes-keybindings.el --- Keybindings for everything  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'joes-utils)

(defun joes-keybindings-common()
	"Set keys used everywhere."
	;; Unset some keybindings that I hate!
	(keymap-global-unset "C-v")
	(keymap-global-unset "M-v")
	(keymap-global-unset "C-x C-z")
	(keymap-unset emacs-lisp-mode-map "C-c C-f")

	(keymap-global-set "C-z" 'undo)
	(keymap-global-set "M-n" (lambda () (interactive) (scroll-up 1)))
	(keymap-global-set "M-p" (lambda () (interactive) (scroll-down 1)))
	(keymap-global-set "C-|" 'joes-toggle-window-split)
	(keymap-global-set "C-<tab>" (lambda () (interactive) (insert-tab)))
	(keymap-global-set "<backtab>" 'indent-according-to-mode)
	(keymap-global-set "C-c C-t" 'xref-find-apropos)
	(keymap-global-set "C-c C-r" 'xref-find-references)
	(keymap-global-set "C-c o" 'imenu)
	(keymap-global-set "s-u" 'revert-buffer-quick)

	(keymap-substitute (current-global-map) 'shell-command 'async-shell)
	(keymap-substitute (current-global-map) 'shell-command 'async-shell-command)
	(keymap-substitute (current-global-map) 'indent-for-tab-command 'joes-indent-or-complete)
	(keymap-substitute (current-global-map) 'c-indent-line-or-region 'joes-indent-or-complete)
	(keymap-substitute (current-global-map) 'list-buffers 'ivy-switch-buffer)

	(keymap-set minibuffer-local-map "<tab>" 'complete-symbol)
	
	(keymap-substitute visual-line-mode-map 'kill-line nil) ;; Remove remapping of kill-line to kill-visual-line

	(keymap-substitute (current-global-map) 'isearch-forward 'isearch-forward-regexp)
	(keymap-substitute (current-global-map) 'isearch-backward 'isearch-backward-regexp))

(defun joes-keybindings-git-commit()
	"Set keys used in git commenting."
	(keymap-substitute (current-local-map) 'indent-for-tab-command 'completion-at-point))

(defun joes-keybindings-python()
	"Set keys used in python mode."
	(declare-function python-indent-line "python")
	(keymap-local-set "C->" (lambda () (interactive)
								   (python-indent-line)))
	(keymap-local-set "C-<" 'python-indent-shift-left))

(defun joes-keybindings-ivy()
	"Set keys used in ivy functions."
	(declare-function ivy-define-key "ivy")
	(defvar ivy-minibuffer-map)
	(keymap-global-set "M-x" 'counsel-M-x)
	(keymap-global-set "C-M-y" 'counsel-yank-pop)
	(keymap-global-set "C-c C-f" 'counsel-git)
	(keymap-global-set "C-M-s" 'counsel-grep-or-swiper)

	(keymap-substitute (current-global-map) 'describe-function 'counsel-describe-function)
	(keymap-substitute (current-global-map) 'describe-variable 'counsel-describe-variable)
	(keymap-substitute (current-global-map) 'ivy-done 'ivy-alt-done)
	(keymap-substitute (current-global-map) 'ivy-partial-or-done 'ivy-partial)
	(keymap-substitute (current-global-map) 'imenu 'counsel-imenu)

	;; minibuffer keys
	(keymap-set ivy-minibuffer-map "M-p" 'ivy-previous-line)
	(keymap-set ivy-minibuffer-map "M-n" 'ivy-next-line)
	(keymap-set ivy-minibuffer-map "C-p" 'ivy-previous-history-element)
	(keymap-set ivy-minibuffer-map "C-n" 'ivy-next-history-element))

(defun joes-keybinding-eglot ()
	"Set keys used for eglot functions."
	(keymap-local-set "C-c C-i" 'eglot-find-implementation)
	(keymap-local-set "C-c C-d" 'eglot-find-declaration)
	(keymap-local-set "C-c C-a" 'eglot-code-actions))

(defun joes-keybinding-ai ()
	"Set keys used for AI assistant."
	(defvar minuet-active-mode-map)
	(keymap-set minuet-active-mode-map "M-n" 'minuet-next-suggestion)
	(keymap-set minuet-active-mode-map "M-p" 'minuet-previous-suggestion)
	(keymap-set minuet-active-mode-map "C-g" 'minuet-dismiss-suggestion)
	(keymap-set minuet-active-mode-map "<tab>" 'minuet-accept-suggestion)
	(keymap-global-set "C-M-/" 'minuet-show-suggestion))

(provide 'joes-keybindings)
;;; joes-keybindings.el ends here
