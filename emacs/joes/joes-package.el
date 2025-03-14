;;; joes-package.el --- joes package.el configuration and functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

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
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
	'(magit-lfs
		 magit
		 pdf-tools
		 pet ;Python environment.
		 jupyter
		 gnu-elpa-keyring-update
		 zenburn-theme
		 logview
		 scad-mode
		 ligature
		 exec-path-from-shell ;Used to work with macos path
		 eglot ;builtin version was too far back.
		 dape ;Debugger using Debug
		 eat ;Terminal emulator
		 gptel ;AI chat
		 minuet ;AI completion suggestion.
		 vertico ;minibuffer completion UI
		 consult ;utilitarian commands based on complete-read - grep, line and completion.
		 orderless ;simple minibuffer completion/order style
		 cape ;usefull to agregate multiple capfs in one. Also cape-file is cool.
		 marginalia ;pretty docs in minibuffer
		 nerd-icons-completion ;pretty font icons in minibuffer
		 ))

(setq treesit-language-source-alist
	'((c . ("https://github.com/tree-sitter/tree-sitter-c"))
		 (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
		 (c-sharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
		 (python . ("https://github.com/tree-sitter/tree-sitter-python"))
		 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))

(defun joes-package-initialize ()
	"Intialize.  Check for required packages."
	(package-initialize)
	(when (and (length> (seq-remove #'package-installed-p package-selected-packages) 0)
 			  (y-or-n-p "Required packages not installed.  Install them?"))
 		(package-refresh-contents)
 		(package-install-selected-packages t)))

(defun joes-package-treesit-init-langs ()
	(let ((langs (seq-filter
					 (lambda (lang)
						 (not (treesit-language-available-p (car lang))))
					 treesit-language-source-alist)))
		(when (and (length> langs 0)
				  (y-or-n-p "Not all tree-sitter grammars are installed.  Install them?"))
			(mapc (lambda (lang-cel)
					  (treesit-install-language-grammar
						  (car lang-cel)
						  (car treesit-extra-load-path))) ;; install where it loads
				langs))))
	

(provide 'joes-package)
;;; joes-package.el ends here
