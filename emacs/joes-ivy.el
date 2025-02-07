;;; joes-ivy.el --- Ivy configuration                -*- lexical-binding: t; -*-

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

(require 'ivy)
(require 'ivy-prescient)
(require 'ivy-xref)
(require 'joes-theme)
(require 'joes-keybindings)
(require 'counsel)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-ivy-switch-buffer-major-mode-column 35
	"Major Mode column shown in `ivy-switch-buffer'."
	:type 'integer)

(defcustom joes-ivy-switch-buffer-path-column 60
	"File path column shown in `ivy-switch-buffer'."
	:type 'integer)

(defcustom joes-ivy-counsel-doc-column 40
	"Column where the documentation should show in `counsel'."
	:type 'integer)

;; Ivy prettify ;)
(defun joes-ivy-switch-buffer-mode-path-transformer (buffer-name)
	"Transformer for `ivy-switch-buffer'.
Add major mode and path for BUFFER-NAME."
	(let ((buffer (get-buffer buffer-name))
			 (result buffer-name))
		(if buffer
			(let ((buffer-major-mode (string-remove-suffix "-mode" (symbol-name (with-current-buffer buffer major-mode))))
					 (buffer-path (buffer-file-name buffer))
					 (name-size (string-width result)))
				(let ((result (concat result
								  (make-string (max 1 (- joes-ivy-switch-buffer-major-mode-column name-size)) ? )
								  (propertize (concat "" buffer-major-mode) 'face 'font-lock-type-face))))
					(let ((result (concat result
									  (make-string (max 1 (- joes-ivy-switch-buffer-path-column (string-width result))) ? )
									  (propertize (concat "" buffer-path) 'face 'font-lock-comment-face))))
						(truncate-string-to-width result (frame-width) nil nil t t))))
			result)))

;; Ivy transformer functions
(defun joes-ivy-counsel-doc-transformer (symbol-name docstring)
	"Transformer for ivy commands that add SYMBOL-NAME DOCSTRING."
	(truncate-string-to-width
		(let ((result
				  (car (split-string
						   (concat
							   symbol-name
							   (make-string (max 1 (- joes-ivy-counsel-doc-column (string-width symbol-name))) ? )
							   (propertize docstring 'face 'font-lock-comment-face))
						   "\n"))))
			result)
		(frame-width) nil nil t t))

(defun joes-ivy-counsel-function-doc-transformer (function-name)
	"Transformer for ivy commands that add FUNCTION-NAME docstring."
	(let* ((func (car (read-from-string function-name)))
			  (key (where-is-internal func nil t))
			  (name-with-keys
				  (concat function-name
					  (when key
						  (propertize
							  (concat " ["
								  (key-description key)
								  "]")
							  'face 'font-lock-type-face)))))
		(joes-ivy-counsel-doc-transformer name-with-keys
			(concat ""
				(when (functionp func)
					(documentation func))))))

(defun joes-ivy-counsel-variable-doc-transformer (variable-name)
	"Transformer for ivy commands that add VARIABLE-NAME docstring."
	(joes-ivy-counsel-doc-transformer variable-name
		(concat ""
			(documentation-property
				(car (read-from-string variable-name)) 'variable-documentation))))

(defun joes-ivy-hook()
  (joes-theme-apply-ivy))

(ivy-mode 1)
(ivy-prescient-mode 1)
(prescient-persist-mode 1)

(setq-default ivy-initial-inputs-alist
	(append
		ivy-initial-inputs-alist
		'((counsel-M-x . "")
			 (counsel-describe-variable . "")
			 (counsel-describe-function . ""))))

(setq-default ivy-use-virtual-buffers t)
(setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
(setq-default xref-show-definitions-function #'ivy-xref-show-defs)
(setq-default xref-show-xrefs-function #'ivy-xref-show-xrefs)

(defadvice completion-at-point (around my-complete act)
	(counsel-company))

;; Set display transformers to prettify ivy.
(ivy-configure 'ivy-switch-buffer :display-transformer-fn 'joes-ivy-switch-buffer-mode-path-transformer)
(ivy-configure 'counsel-M-x :display-transformer-fn 'joes-ivy-counsel-function-doc-transformer)
(ivy-configure 'counsel-describe-variable :display-transformer-fn 'joes-ivy-counsel-variable-doc-transformer)
(ivy-configure 'counsel-describe-function :display-transformer-fn 'joes-ivy-counsel-function-doc-transformer)
(ivy-configure t :format-fn 'ivy-format-function-line)

(joes-keybindings-ivy)

(provide 'joes-ivy)
;;; joes-ivy.el ends here
