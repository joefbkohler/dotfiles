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
	(global-unset-key (kbd "C-v"))
	(global-unset-key (kbd "M-v"))
	(global-unset-key (kbd "C-x C-z"))

	(global-set-key (kbd "C-z") 'undo)
	(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
	(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
	(global-set-key (kbd "C-|") 'joes-toggle-window-split)
	(global-set-key (kbd "C-<tab>") (lambda () (interactive) (insert-tab)))
	(global-set-key [remap shell-command] 'async-shell-command)
	(global-set-key [remap indent-for-tab-command] 'joes-indent-or-complete)
	(global-set-key [remap c-indent-line-or-region] 'joes-indent-or-complete)
	(global-set-key (kbd "<backtab>") 'indent-according-to-mode)
	(global-set-key [remap list-buffers] 'ivy-switch-buffer)
	(global-set-key (kbd "C-c C-t") 'xref-find-apropos)
	(global-set-key (kbd "C-c C-r") 'xref-find-references)
	(global-set-key (kbd "C-c o") 'imenu)
	(global-set-key (kbd "s-u") 'revert-buffer-quick)

	(define-key minibuffer-local-map (kbd "<tab>") 'complete-symbol)
	;; Remove remapping of kill-line to kill-visual-line
	(define-key visual-line-mode-map [remap kill-line] nil)

	(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
	(global-set-key [remap isearch-backward] 'isearch-backward-regexp))

(defun joes-keybindings-git-commit()
	"Set keys used in git commenting."
	(local-set-key [remap indent-for-tab-command] 'completion-at-point))

(defun joes-keybindings-python()
	"Set keys used in python mode."
	(declare-function python-indent-line "python")
	(local-set-key (kbd "C->") (lambda () (interactive)
								   (python-indent-line)))
	(local-set-key (kbd "C-<") 'python-indent-shift-left))

(defun joes-keybindings-ivy()
	"Set keys used in ivy functions."
	(declare-function ivy-define-key "ivy")
	(defvar ivy-minibuffer-map)
	(global-set-key (kbd "M-x") 'counsel-M-x)
	(global-set-key [remap describe-function] 'counsel-describe-function)
	(global-set-key [remap describe-variable] 'counsel-describe-variable)
	(global-set-key (kbd "C-M-y") 'counsel-yank-pop)
	(global-set-key (kbd "C-c C-f") 'counsel-git)
	(global-set-key [remap isearch-forward-regexp] 'counsel-grep-or-swiper)
	(global-set-key [remap ivy-done] 'ivy-alt-done)
	(global-set-key [remap ivy-partial-or-done] 'ivy-partial)
	(global-set-key [remap imenu] 'counsel-imenu)

	;; minibuffer keys
	(ivy-define-key ivy-minibuffer-map (kbd "M-p") 'ivy-previous-line)
	(ivy-define-key ivy-minibuffer-map (kbd "M-n") 'ivy-next-line)
	(ivy-define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-history-element)
	(ivy-define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-history-element))

(defun joes-keybinding-eglot ()
	"Set keys used for eglot functions."
	(local-set-key (kbd "C-c C-i") 'eglot-find-implementation)
	(local-set-key (kbd "C-c C-d") 'eglot-find-declaration)
	(local-set-key (kbd "C-c C-a") 'eglot-code-actions))

(provide 'joes-keybindings)
;;; joes-keybindings.el ends here
