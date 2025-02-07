;;; joes-latex.el --- Latex configuration            -*- lexical-binding: t; -*-

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

(require 'tex-mode)
(require 'joes-utils)
(require 'joes-company)

(defun joes-latex-mode-hook ()
	"Latex mode hook configuration."
	(local-set-key [remap tex-compile] 'joes-tex-compile-update)
	(add-to-list 'company-capf-prefix-functions 'my-latex-company-capf-prefix)
	(eglot-ensure)
	(flyspell-mode 1)
	(flymake-mode 1)
	(set-fill-column 63)
	(auto-fill-mode 1))

(defun joes-latex-compile-update()
	"Compile to PDF, update buffer and show in window."
	(interactive)
	(when (and (string= (buffer-name) (tex-main-file))
			  (not (string-match-p (regexp-quote "documentclass") (buffer-string))))
		(error "%s" "Main file buffer with documentclass not found. Is it open?"))

	(if (tex-shell-running)
		(tex-kill-job)
		(tex-start-shell))

	(when (< (count-windows) 2)
		(split-window-right))

	(let* ((makefile (joes-search-file-regex-upward "Makefile"))
			  (path (file-name-directory makefile)))
		(if (string= makefile "")
			(progn
				;; No Makefile. Gotta run twice to create table of contents. But first time can be draft mode.
				(shell-command (concat "lualatex --draftmode --halt-on-error" " " (tex-main-file)))
				(compile (concat "lualatex --halt-on-error" " " (tex-main-file)))
				(setq path "./"))
			(shell-command (concat "cd " path " && make")))
		(let* ((pdf-file-name (replace-regexp-in-string "tex$" "pdf" (tex-main-file)))
				  (pdf-buffer (get-buffer pdf-file-name)))
			(if pdf-buffer
				(progn
					(switch-to-buffer-other-window pdf-buffer)
					(revert-buffer :noconfirm t))
				(find-file-other-window
					(car (split-string (shell-command-to-string
										   (concat "find " path " -name " pdf-file-name)))))))))

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
