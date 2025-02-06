;;; package --- Joe's company functions and configuration
;;; Commentary:
;; My custom functions for ispell dictionaries.
;;; Code:

(require 'joes-ispell)
(require 'company)

(defgroup joe nil
	"My little functions"
	:group 'convenience)

(defcustom company-capf-prefix-functions '(joes-company-capf-prefix)
	"Functions that return t if current position should skip `company-capf'."
	:type 'list)
(make-variable-buffer-local 'company-capf-prefix-functions)

(defun joes-company-capf-extra-prefix-check (orig-fun command &optional arg &rest _args)
	(when
		(not (seq-some (lambda (func)
						   (funcall func))
				 company-capf-prefix-functions))
		(apply orig-fun command arg _args)))

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
	'(company-capf company-files (company-ispell company-dabbrev)))

(when (not (ispell-lookup-words "WHATEVER"))
	(warn "Autocomplete using dictionary will not work correctly. You  have to create a `words' file. See: ispell-change-dictionary-and-words. Restart emacs afterwards."))

(global-company-mode 1)

(provide 'joes-company)
;;; joes-company.el ends here
