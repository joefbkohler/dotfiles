
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
		 yaml-mode
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
		 marginalia ;minibuffe pretty docs
		 consult ;utilitarian commands based on complete-read - grep, line and completion.
		 orderless ;simple minibuffer completion/order style
		 ))

(defun joes-package-initialize ()
	"Intialize.  Check for required packages."
	(package-initialize)
	(when (and (length> (seq-remove #'package-installed-p package-selected-packages) 0)
 			  (y-or-n-p "Required packages not installed.  Install them?"))
 		(package-refresh-contents)
 		(package-install-selected-packages t)))

(provide 'joes-package)
;;; joes-package.el ends here
