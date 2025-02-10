;;; joes-company.el --- Joe's company functions and configuration  -*- lexical-binding: t; -*-

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

(require 'joes-ispell)
(require 'company)

(defgroup joe nil
	"My little functions."
	:group 'convenience)

(defcustom company-capf-prefix-functions '(joes-company-capf-prefix)
	"Functions that return t if current position should skip `company-capf'."
	:type 'list)
(make-variable-buffer-local 'company-capf-prefix-functions)

(defun joes-company-capf-extra-prefix-check (orig-fun command &optional arg &rest args)
	"Advice to `company-capf' to better decide when to call capf or dabbrev/ispell.
ORIG-FUN is the capf function.  COMMAND, ARG and ARGS are the
arguments passed to the capf."
	(when
		(not (seq-some (lambda (func)
						   (funcall func))
				 company-capf-prefix-functions))
		(apply orig-fun command arg args)))

(defun joes-company-capf-prefix ()
	"Check if current prefix is a valid `company-capf' prefix."
	(when (or
			  (nth 3 (syntax-ppss))
			  (nth 4 (syntax-ppss)))
		t))

(advice-add 'company-capf :around 'joes-company-capf-extra-prefix-check)

(setq-default company-dabbrev-ignore-case 'keep-prefix)
(setq-default company-idle-delay nil)
(setq-default company-backends
	'(company-capf company-files (company-dabbrev company-ispell :separate)))

(when (not (ispell-lookup-words "WHATEVER"))
	(warn "Autocomplete using dictionary will not work correctly. You  have to create a `words' file. See: ispell-change-dictionary-and-words. Restart emacs afterwards."))

(global-company-mode 1)

(provide 'joes-company)
;;; joes-company.el ends here
