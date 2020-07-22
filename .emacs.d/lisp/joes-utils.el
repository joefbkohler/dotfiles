;;; package --- Joe's Utils
;;; Commentary:
;; My custom functions.  Either made by me or stolen from the internet ;)
;;; Code:

(defun blink-minibuffer (&optional time)
	"Blink the minibuffer for a set TIME."
	(unless time (setq time 0.1))
	(invert-face 'mode-line)
	(run-with-timer time nil #'invert-face 'mode-line))

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

(defun vc-dir-delete-marked-files ()
	"Delete all marked files in a `vc-dir' buffer."
	(interactive)
	(let ((files (vc-dir-marked-files)))
		(if (not files)
			(message "No marked files.")
			(when (yes-or-no-p (format "%s %d marked file(s)? "
								   (if delete-by-moving-to-trash "Trash" "Delete")
								   (length files)))
				(unwind-protect
					(mapcar
						(lambda (path)
							(if (and (file-directory-p path)
									(not (file-symlink-p path)))
								(when (or (not (directory-files
												   path nil directory-files-no-dot-files-regexp))
										  (y-or-n-p
											  (format "Directory `%s' is not empty, really %s? "
												  path (if delete-by-moving-to-trash
														   "trash" "delete"))))
									(delete-directory path t t))
								(delete-file path t)))
						files)
					(revert-buffer))))))

(defun toggle-window-split ()
	(interactive)
	(if (= (count-windows) 2)
		(let* ((this-win-buffer (window-buffer))
				  (next-win-buffer (window-buffer (next-window)))
				  (this-win-edges (window-edges (selected-window)))
				  (next-win-edges (window-edges (next-window)))
				  (this-win-2nd (not (and (<= (car this-win-edges)
											  (car next-win-edges))
										 (<= (cadr this-win-edges)
											 (cadr next-win-edges)))))
				  (splitter
					  (if (= (car this-win-edges)
							  (car (window-edges (next-window))))
						  'split-window-horizontally
						  'split-window-vertically)))
			(delete-other-windows)
			(let ((first-win (selected-window)))
				(funcall splitter)
				(if this-win-2nd (other-window 1))
				(set-window-buffer (selected-window) this-win-buffer)
				(set-window-buffer (next-window) next-win-buffer)
				(select-window first-win)
				(if this-win-2nd (other-window 1))))))

;; Global adaptive-wrap-prefix-mode. Why doesn't this exist by default??? õO
(define-global-minor-mode global-adaptive-wrap-prefix-mode
	adaptive-wrap-prefix-mode
	(lambda() (adaptive-wrap-prefix-mode 1)))

;; Omnisharp
(defun omnisharp-navigate-to-solution-type (&optional other-window)
	"Interactively navigate to a type in the solution.  If OTHER-WINDOW is not nil, navigate in other window."
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
			other-window)
		))

(defun omnisharp-find-usages-visuals ()
	"Remove redundant information from omnisharp find usages buffer like the full path of the file."
	(when (string-match "OmniSharp" (buffer-name))
		
		(font-lock-add-keywords nil '(("^/.*/" (0 '(face default display ".../") append))) t)
		(font-lock-add-keywords nil '(("^[ \t]*" (0 '(face default display "") append))) t)
		
		(add-function :before-until (local 'eldoc-documentation-function)
			(lambda ()
				"Show the complete filename, line and column of the match."
				(let* ((line-text (replace-regexp-in-string "\n$" "" (thing-at-point 'line t)))
						 (current-line-match (string-match "\.cs" line-text)))
					(when (and (not current-line-match) (not (= (length line-text) 0)))
						(forward-line -1)
						(setq line-text (replace-regexp-in-string "\n$" "" (thing-at-point 'line t)))
						(forward-line 1))
					line-text)))
		(eldoc-mode)
		))

(defun clear-line-end ()
	"Clear all wrong line ends."
	(interactive)
	(save-excursion
		(goto-char (point-min))
		(while (re-search-forward "" nil t)
			(replace-match ""))
		))

;;Grep / usages / compilation mode truncate
;;(font-lock-add-keywords 'compilation-mode '(("^/.*/" (0 '(face default display "...") append))) t)

(provide 'joes-utils)
;;; joes-utils.el ends here
