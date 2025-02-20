;;; joes-latex.el --- Latex configuration            -*- lexical-binding: t; -*-

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

(require 'tex-mode)
(require 'joes-utils)
(require 'joes-company)
(require 'joes-keybindings)
(require 'project)

(defun joes-latex-mode-hook ()
	"Latex mode hook configuration."
	(joes-latex-keybinding)
	(add-to-list 'company-capf-prefix-functions 'joes-latex-company-capf-prefix)
	(eglot-ensure)
	(flyspell-mode 1)
	(flymake-mode 1)
	(set-fill-column 63)
	(auto-fill-mode 1))

(defun joes-latex-find-main-file()
	"Find and open main TeX file."
	(when (not (tex-main-file))
		(let* ((default-directory (project-root (project-current)))
				  (tex-root-file-path
					  (string-trim (shell-command-to-string "find . -iname '*.tex' -exec grep -l documentclass {} +"))))
			(find-file-noselect tex-root-file-path))))

(defun joes-latex-show-project-pdf ()
	"Find the compiled pdf and show it in a split window to the right."
	(interactive)
	(when (< (count-windows) 2)
		(split-window-right))
	(joes-latex-find-main-file)
	(let* ((pdf-file-name (replace-regexp-in-string "tex$" "pdf" (tex-main-file)))
	 		  (pdf-buffer (get-buffer pdf-file-name)))
		(message "what window is this? %s" (current-buffer))
		(save-selected-window
			(if pdf-buffer
	 			(progn
	 				(switch-to-buffer-other-window pdf-buffer)
	 				(revert-buffer :noconfirm t))
				(find-file-other-window
	 				(car (split-string
							 (shell-command-to-string
	 							 (concat "find " (project-root (project-current)) " -name " pdf-file-name)))))))))

(defun joes-latex-compile-project ()
	"Ask to save relevant files.  Try to run Makefile.  Compile to PDF."
	(interactive)
	(let ((project (project-current)))
		(save-some-buffers nil
			(lambda()
				(memq (current-buffer) (project-buffers project)))))

	(let* ((makefile (joes-search-file-regex-upward "Makefile"))
			  (default-directory (file-name-directory makefile))
			  (tex-root-file (tex-main-file)))
		(if (string= makefile "")
			(progn
				(when (and (string= (buffer-name) (tex-main-file))
						  (not (string-match-p (regexp-quote "documentclass") (buffer-string))))
					(error "%s" "Main file buffer with documentclass not found. Is it open?"))
				;; No Makefile. Gotta run twice to create table of contents. But first time can be draft mode.
				(shell-command (concat "lualatex --draftmode --halt-on-error" " " tex-root-file))
				(shell-command (concat "lualatex --halt-on-error" " " tex-root-file)))
			(shell-command "make"))))

(defun joes-latex-compile-and-show()
	"Compile than show pdf."
	(interactive)
	(joes-latex-compile-project)
	(joes-latex-show-project-pdf))

(defun joes-latex-company-capf-prefix()
	"Check if current prefix is a valid `company-capf' prefix in `latex-mode'."
	(save-excursion
		(let* ((start-point (point))
			      (pos-slash (search-backward "\\" nil t))
				  (pos-bracket (search-forward "{" nil t)))
			(when (or (not pos-slash)
					  (and pos-bracket
						  (<= pos-bracket start-point)))
				t))))

(provide 'joes-latex)
;;; joes-latex.el ends here
