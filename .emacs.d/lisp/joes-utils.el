;; Custom functions

;;; Code:
(defun smarter-move-beginning-of-line (arg)
	"Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
	(interactive "^p")
	(setq arg (or arg 1))

	;; Move lines first
	(when (/= arg 1)
		(let ((line-move-visual nil))
			(forward-line (1- arg))))

	(let ((orig-point (point)))
		(back-to-indentation)
		(when (= orig-point (point))
			(move-beginning-of-line 1))))

(defun indent-or-complete ()
	"Try to indent.  If line is already indented, invoke company-complete."
	(interactive)
	(if mark-active

		(indent-for-tab-command)

		(let ((initial-point (point)))
			(indent-for-tab-command)
			(when (eq initial-point (point))
				(company-complete)
				))
		)
	)

;; Omnisharp
(defun omnisharp-navigate-to-solution-type
	(&optional other-window)
	(interactive "P")
	(require 'omnisharp)
	(let ((quickfix-response
			  (omnisharp-post-message-curl-as-json
				  (concat (omnisharp-get-host) "findtypes")
				  nil)))
		(omnisharp--choose-and-go-to-quickfix-ido
			(mapcar 'omnisharp-format-symbol
				(cdr (omnisharp--vector-to-list
						 (cdr (assoc 'QuickFixes quickfix-response)))))
			other-window)))

(defun omnisharp-grep-solution ()
	(interactive)
	(require 'omnisharp)
	(let ((search-query (read-string "Search solution: " "")))
		(grep (concat "grep -niH --color -e " (concat (concat search-query " ") (mapconcat 'identity (omnisharp--get-solution-files-list-of-strings) " "))))
		)
	)

;; Fix for strange bug where the message that comes from the server is out of order
(defun around-omnisharp-format-symbol (orig-fun item)
	"Remove first item and puts last item as first of the list."
	(apply orig-fun (list (cons (car (last (cdr item))) (butlast (cdr item)))))
	)

(advice-add 'omnisharp-format-symbol :around #'around-omnisharp-format-symbol)

;; Grep / usages / compilation mode truncate
;; (font-lock-add-keywords 'compilation-mode '(("^/.*/" (0 '(face default display "...") append))) t)

(provide 'joes-utils)
;;; joes-utils.el ends here
