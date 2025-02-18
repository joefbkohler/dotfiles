;;; joes-mode-line.el --- Mode-line layout and configuration  -*- lexical-binding: t; -*-

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

(require 'vc-git)
(require 'joes-utils)

(defvar-local joes-mode-line-vc-branch ""
	(put 'joes-mode-line-vc-branch 'risky-local-variable t))

(defun joes-update-mode-line-vc ()
	"Update VC branch and modified in mode-line."
	(let ((branch (car (vc-git-branches))))
		(if branch
			(joes-async-shell-command-to-string
				(lambda (result)
					(setq joes-mode-line-vc-branch
						(format " %s %s"
							(propertize "" 'face
								(if (string-empty-p result) '(:foreground nil) '(:foreground "#A22")))
							branch)))
				"git" "status" "--porcelain" "-z")
			(setq joes-mode-line-vc-branch ""))))

(setq-default mode-line-format
	'(:eval
		 (joes-simple-mode-line-render
			 '("%e "
				  (:eval (if buffer-read-only "󰷪" "󰲶"))
				  " "
				  (:eval (if (file-remote-p default-directory) "󰲁" "󰉖"))
				  " "
				  (:eval (if (buffer-modified-p) "󰷈" "󱪚"))
				  "  "
				  mode-line-buffer-identification
				  mode-line-process
				  (:eval joes-mode-line-vc-branch)
				  (:eval (when (bound-and-true-p flymake-mode) (concat " " (format-mode-line flymake-mode-line-format))))
				  (:eval (when (not (string-empty-p (format-mode-line mode-line-misc-info)))
							 (concat " " (format-mode-line mode-line-misc-info)))))
			 
			 '(""
				  (:eval (if (<= (count-windows) 1) (concat (format-mode-line mode-name) "  ")))
				  "[%3l:%3C]"
				  mode-line-percent-position
				  " "))))

(run-at-time nil 0.5 'joes-update-mode-line-vc)

(provide 'joes-mode-line)
;;; joes-mode-line.el ends here

