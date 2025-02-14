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
                  (:eval (when (car (vc-git-branches))
                             (format " %s %s" (propertize "" 'face
                                                  (if (joes-is-git-worktree-clean) '(:foreground nil) '(:foreground "#A22")))
                                 (car (vc-git-branches)))))
			      (:eval (when (bound-and-true-p flymake-mode) (concat " " (format-mode-line flymake-mode-line-format))))
                  (:eval (when (not (string-empty-p (format-mode-line mode-line-misc-info)))
                             (concat " " (format-mode-line mode-line-misc-info)))))
             
		     '(""
                  (:eval (if (<= (count-windows) 1) (concat (format-mode-line mode-name) "  ")))
                  "[%3l:%3C]"
                  mode-line-percent-position
                  " "))))

(provide 'joes-mode-line)
;;; joes-mode-line.el ends here
