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
	(keymap-global-set "C-c C-f" 'project-find-file)

	(keymap-substitute (current-global-map) 'shell-command 'async-shell)
	(keymap-substitute (current-global-map) 'shell-command 'async-shell-command)
	(keymap-substitute (current-global-map) 'indent-for-tab-command 'joes-indent-or-complete)
	(keymap-substitute (current-global-map) 'c-indent-line-or-region 'joes-indent-or-complete)
		
	(keymap-substitute visual-line-mode-map 'kill-line nil) ;; Remove remapping of kill-line to kill-visual-line

	(keymap-substitute (current-global-map) 'isearch-forward 'isearch-forward-regexp)
	(keymap-substitute (current-global-map) 'isearch-backward 'isearch-backward-regexp))

(defun joes-keybindings-eat()
	"Set eat keybindings."
	(keymap-global-set "M-RET" 'eat-other-window)
	(keymap-global-set "C-M-<return>" 'eat-project-other-window))

(defun joes-keybindings-git-commit()
	"Set keys used in git commenting."
	(defvar git-commit-mode-map)
	(keymap-set git-commit-mode-map "<tab>" 'completion-at-point))

(defun joes-keybindings-python()
	"Set keys used in python mode."
	(declare-function python-indent-line "python")
	(keymap-local-set "C->" (lambda () (interactive)
								   (python-indent-line)))
	(keymap-local-set "C-<" 'python-indent-shift-left))

(defun joes-keybindings-vertico()
	"Set keys used in vertico functions."
	(keymap-set vertico-map "C-M-p" 'vertico-previous-group)
	(keymap-set vertico-map "C-M-n" 'vertico-next-group)
	(keymap-set vertico-map "DEL" 'vertico-directory-delete-char)
    (keymap-set vertico-map "<TAB>" 'minibuffer-complete)
    (keymap-set vertico-map "<RET>" 'vertico-directory-enter))

(defun joes-keybindings-consult()
	"Set keys used in Consult functions."
	(keymap-global-set "C-M-s" 'consult-line)
    (keymap-global-set "C-M-y" 'consult-yank-pop)
	(keymap-substitute (current-global-map) 'switch-to-buffer 'consult-buffer)
    (keymap-substitute (current-global-map) 'list-buffers 'consult-buffer)
    (setopt consult-preview-key "C-v"))

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

(defun joes-latex-keybinding ()
	"Keys for TeX mode."
	(defvar latex-mode-map)
	(keymap-substitute latex-mode-map 'tex-compile 'joes-latex-compile-and-show))

(joes-keybindings-common)

(provide 'joes-keybindings)
;;; joes-keybindings.el ends here
