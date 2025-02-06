;;; joes-utils.el --- Collection of useful functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Joe Köhler

;; Author: Joe Köhler <joe.fb.kohler@gmail.com>
;; Keywords: 

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

(defun joes-header-has-c++-implementation (file-name)
	(or (joes-search-file-regex-upward
					(concat (file-name-nondirectory
								(file-name-sans-extension file-name))
						".cpp") 2 t)
				(joes-search-file-regex-upward
					(concat (file-name-nondirectory
								(file-name-sans-extension file-name))
						".cc") 2 t)))

(defun joes-blink-minibuffer (&optional time)
	"Blink the minibuffer for a set TIME."
	(unless time (setq time 0.1))
	(invert-face 'mode-line)
	(run-with-timer time nil #'invert-face 'mode-line))

(defun joes-create-scratch-buffer ()
	"Create a new scratch buffer if one does not exist."
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(lisp-interaction-mode))

(defun joes-buffer-indentation-offset ()
	(save-excursion
		(if (search-forward-regexp "^\t+[^[:blank:]]" nil t)
			(current-indentation)
			(if (search-forward-regexp "^\s+[^[:blank:]]" nil t)
				(current-indentation)
				tab-width))))

(defun joes-search-file-regex-upward (file-regexp &optional depth recursive)
	"Find FILE-REGEXP moving up folders up to DEPTH.  RECURSIVE."
	(let ((num 0) (file-path "") (cur-path ".")
			 (depth (or depth 2))
			 (recursive (number-to-string (if recursive (+ depth 1) 1))))
		(while (< num depth)
			(setq file-path (shell-command-to-string (concat "find " cur-path " -maxdepth " recursive " -name \"" file-regexp "\"")))
			(setq cur-path (concat "../" cur-path))
			(setq num (1+ num))
			(when (not (string= file-path ""))
				(setq num 999999)))
		(when (not (string= file-path ""))
			(file-truename file-path))))

(defun joes-indent-or-complete ()
	"Try to indent.	 If line is already indented, invoke `completion-at-point'."
	(interactive)
	(if mark-active
		(indent-for-tab-command)
		(let ((initial-indentation (current-indentation))
				 (initial-position (point)))
			(indent-for-tab-command)
			(when (and
					  (eq initial-position (point))
					  (eq initial-indentation (current-indentation)))
				(completion-at-point)))))

(defun joes-tree-sitter-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix in `tree-sitter'."
	(save-excursion
		(ignore-errors (backward-char))
		(let* ((node-type (tsc-node-type (tree-sitter-node-at-pos :named))))
			(when (or (string-match-p "comment" (pp-to-string node-type))
					  (string-match-p "string" (pp-to-string node-type)))
				t))))

(defun joes-multi-replace-regexp-in-string (replace-pairs string)
	"Replace in STRING all keys by the values in REPLACE-PAIRS."
	(seq-reduce
		(lambda (string replace-pair)
			(replace-regexp-in-string
				(concat (car replace-pair) "+")
				(cdr replace-pair)
				string
				t))
		replace-pairs
		string))

(defun joes-toggle-window-split ()
	"Toggle two windows from vertical to horizontal split and vice versa."
	(interactive)
	(if (= (count-windows) 2)
		(let* ((this-win-buffer (window-buffer))
				  (next-win-buffer (window-buffer (next-window)))
				  (this-win-edges (window-edges (selected-window)))
				  (next-win-edges (window-edges (next-window)))
				  (this-win-2nd (not (and (<= (car this-win-edges)
											  (car next-win-edges))
										 (<= (cadr this-win-edges)
											 (cadr next-win-edges)))))
				  (splitter
					  (if (= (car this-win-edges)
							  (car (window-edges (next-window))))
						  'split-window-horizontally
						  'split-window-vertically)))
			(delete-other-windows)
			(let ((first-win (selected-window)))
				(funcall splitter)
				(if this-win-2nd (other-window 1))
				(set-window-buffer (selected-window) this-win-buffer)
				(set-window-buffer (next-window) next-win-buffer)
				(select-window first-win)
				(if this-win-2nd (other-window 1))))))

(provide 'joes-utils)
;;; joes-utils.el ends here
