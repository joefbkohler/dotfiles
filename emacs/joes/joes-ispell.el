;;; joes-ispell.el --- ispell configuration          -*- lexical-binding: t; -*-

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

(require 'ispell)
(require 'cl-lib)
(require 'recentf)

;; Prefer Hunspell. If not, whatever is found.
(setopt ispell-program-name (or (executable-find "hunspell") ispell-program-name))
(setopt ispell-complete-word-dict (file-truename "~/.words"))

(defun joes-ispell-aspell-words (dict)
	"Return all words of an aspell DICT."
	(string-join
		(cl-mapcar
			(lambda (word)
				(replace-regexp-in-string "\/.+" "" word))
			(sort (split-string
					  (shell-command-to-string
						  (concat "aspell dump master " (car (split-string dict "-")))))
				'string-lessp))
		"\n"))

(defun joes-ispell-hunspell-words (dict)
	"Return all words of an hunspell DICT."
	(let ((dict-path
			  (car (seq-filter
					   (lambda (path) (string-equal dict (file-name-nondirectory path)))
					   (split-string (shell-command-to-string "hunspell -D"))))))
		(shell-command-to-string (concat "unmunch " dict-path ".dic " dict-path ".aff"))))

(defun joes-ispell-available-dicts ()
	"If hunspell in use, return only found dicts."
	(if ispell-really-hunspell
		(seq-filter (lambda (dict)
						(string-match-p (regexp-quote dict)
							(shell-command-to-string "hunspell -D")))
			(ispell-valid-dictionary-list))
		(ispell-valid-dictionary-list)))

(defun joes-ispell-change-dictionary-and-words ()
	"Switch Ispell dictionary and create words file."
	(interactive)
	(let* ((perm-excluded recentf-exclude)
			  (new-dict (completing-read
							"Use new dictionary: "
							(mapcar #'list (joes-ispell-available-dicts))
							nil t))
			  (words
				  (if ispell-really-hunspell
					  (joes-ispell-hunspell-words new-dict)
					  (joes-ispell-aspell-words new-dict))))
		(ispell-change-dictionary new-dict)
		(with-current-buffer
			(find-file ispell-complete-word-dict)
			(erase-buffer)
			(insert words)
			(save-buffer)
			(kill-buffer))
		(add-to-list 'recentf-exclude ispell-complete-word-dict)
		(recentf-cleanup)
		(setq recentf-exclude perm-excluded)))

(provide 'joes-ispell)
;;; joes-ispell.el ends here
