;;; joes-prog.el --- prog modes configs and hooks    -*- lexical-binding: t; -*-

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

(message "joes prog config file loaded")
(defun joes-prog-mode-hook ()
	"General prog mode config."
	;; Look for a tab indentation, if found, set indent-tabs-mode.
	(message "%s" "run prog mode?")
	(setq indent-tabs-mode (when (not (string-match
										  "^\s+[^[:blank:]]"
										  (buffer-substring-no-properties 1 (point-max)))) t))
	(flymake-mode 1)
	(display-line-numbers-mode 1)
	(setq-local display-line-numbers-grow-only 1)
	
	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode -1)

	(setq-local company-dabbrev-ignore-case nil)
	(setq-local company-dabbrev-downcase nil))

(defun joes-ediff-mode-hook()
	"Ediff mode config."
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))

(defun joes-elisp-mode-hook ()
	"Elisp mode config."
	(setq-local lisp-indent-offset (or (joes-buffer-indentation-offset) 4)))

(defun joes-text-mode-hook ()
	"Text mode config."
	(buffer-face-mode))

(defun joes-c-mode-common-hook ()
	"C-common mode config."
	(eglot-ensure)
	(c-set-style "csharp")
	(setq-local c-indent-offset (joes-buffer-indentation-offset)))

(defun joes-c-mode-hook ()
	"C-mode config."
	(if (and (string-suffix-p ".h" (buffer-file-name) t)
			(joes-header-has-c++-implementation buffer-file-name))
		(c++-mode)))

(defun joes-python-mode-hook ()
	"Python mode config."
	(declare-function pet-mode "pet")
	(setq indent-tabs-mode nil)
	(pet-mode)
	(joes-keybindings-python)
	(eglot-ensure))

(provide 'joes-prog)
;;; joes-prog.el ends here
