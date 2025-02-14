
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
	'(ivy-xref
		 counsel
		 ivy-prescient
		 magit-lfs
		 magit
		 pdf-tools
		 pet
		 yaml-mode
		 dockerfile-mode
		 jupyter
		 gnu-elpa-keyring-update
		 ivy
		 zenburn-theme
		 logview
		 company
		 scad-mode
		 ligature
		 exec-path-from-shell
		 eglot
		 dape
		 eat
		 gptel
		 minuet))

(package-initialize)

(when (and (length> (seq-remove #'package-installed-p package-selected-packages) 0)
		  (y-or-n-p "Required packages not installed.  Install them?"))
	(package-refresh-contents)
	(package-install-selected-packages t))

(provide 'joes-package)
;;; joes-package.el ends here
