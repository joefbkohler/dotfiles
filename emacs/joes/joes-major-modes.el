;;; joes-major-modes.el --- prog modes configs and hooks    -*- lexical-binding: t; -*-

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

;;; Code:
(require 'whitespace)
(require 'joes-utils)
(require 'joes-keybindings)

(defun joes-prog-mode-config ()
	"General prog mode config."
	;; Look for a tab indentation, if found, set indent-tabs-mode.
	(setq indent-tabs-mode
		  (when (not (string-match
					  "^\s+[^[:blank:]]"
					  (buffer-substring-no-properties 1 (point-max)))) t))
	
	(hl-line-mode)
	(declare-function flymake-eldoc-function "flymake")
	(flymake-mode 1)
	(push #'flymake-eldoc-function eldoc-documentation-functions)

	(setq-local display-line-numbers-width-start t)
	(display-line-numbers-mode 1)

	(setq-local fill-column 79)
	(display-fill-column-indicator-mode t)
	
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode -1))

(defun joes-ediff-mode-config()
	"Ediff mode config."
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))

(defun joes-elisp-mode-config ()
	"Elisp mode config."
	(setq-local lisp-indent-offset (or (joes-buffer-indentation-offset) 4)))

(defun joes-text-mode-config ()
	"Text mode config."
	(buffer-face-mode)
	(hl-line-mode))

(defun joes-c-mode-common-config ()
	"C-common mode config."
	(eglot-ensure)
	(setq-local c-indent-offset (joes-buffer-indentation-offset))
	(setq-local c-ts-mode-indent-offset (joes-buffer-indentation-offset)))

(defun joes-python-mode-config ()
	"Python mode config."
	(declare-function pet-mode "pet")
	(setq indent-tabs-mode nil)
	(pet-mode)
	(joes-keybindings-python)
	(eglot-ensure))

(defun joes-eglot-config ()
	"Eglot config."
	(declare-function eglot-inlay-hints-mode "eglot")
	(joes-keybinding-eglot)
	(eglot-inlay-hints-mode -1))

(provide 'joes-major-modes)
;;; joes-major-modes.el ends here
