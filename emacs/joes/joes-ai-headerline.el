;;; joes-ai-headerline.el --- gptel header line for AI chat  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Joe Köhler

;; Author: Joe Köhler <joe.fb.kohler@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Replaces the info segment of the gptel header line with preset, last
;; request token usage, session cost, context, tools, media and model.

;;; Code:

(require 'gptel)
(require 'gptel-context)

(defvar-local joes-ai-gptel--turn-usage nil
	"Token usage plist of the last turn in this gptel buffer.")

(defun joes-ai-gptel--record-turn-usage (tokens &rest _)
	"Record TOKENS, the usage of the latest turn, as the context usage."
	(when tokens
		(setq joes-ai-gptel--turn-usage tokens)))

(defun joes-ai-gptel--format-tokens (count)
	"Return token COUNT as a number, or in thousands (k) from 1000 up."
	(if (< count 1000)
		(number-to-string count)
		(format "%dk" (round (/ count 1000.0)))))

(defun joes-ai-gptel--header-usage ()
	"Return context usage of the last turn: input, output, total, and window."
	(when-let* ((last joes-ai-gptel--turn-usage)
				(input (plist-get last :input))
				(output (plist-get last :output)))
		(let ((window (get gptel-model :context-window)))
			(concat "[" (joes-ai-gptel--format-tokens input) "↑ "
				(joes-ai-gptel--format-tokens output) "↓ = "
				(joes-ai-gptel--format-tokens (+ input output))
				(and window
					(format " of %s" (joes-ai-gptel--format-tokens window)))
				"]"))))

(defun joes-ai-gptel--header-cost ()
	"Return the session cost estimated from the model prices, or nil."
	(when-let* ((session (cadr gptel--token-usage))
				(input-cost (get gptel-model :input-cost))
				(output-cost (get gptel-model :output-cost)))
		(format "[$%.4f]"
			(/ (+ (* (or (plist-get session :input) 0) input-cost)
				  (* (or (plist-get session :output) 0) output-cost))
				1e6))))

(defvar joes-ai-gptel--header-line-info
	'(:eval
		(let* ((model (gptel--model-name gptel-model))
				(preset
					(propertize
						(if gptel--preset
							(format "[Preset: %s]" gptel--preset)
						"[No Preset]")
						'mouse-face 'highlight
						'help-echo "Preset applied to this buffer"))
				(usage
					(when-let* ((text (joes-ai-gptel--header-usage)))
						(propertize text
							'mouse-face 'highlight
							'help-echo "Last request: input, output, total, context window")))
				(cost
					(when-let* ((text (joes-ai-gptel--header-cost)))
						(propertize text
							'mouse-face 'highlight
							'help-echo "Session cost (estimated from model prices)")))
				(context
					(and gptel-context
						(cl-loop
							for entry in gptel-context
							if (bufferp (or (car-safe entry) entry)) count it into bufs
							else count (stringp (or (car-safe entry) entry)) into files
							finally return
							(propertize
								(buttonize
									(concat "[" (and (> bufs 0) (format "%d buf" bufs))
										(and (> bufs 1) "s")
										(and (> bufs 0) (> files 0) ", ")
										(and (> files 0) (format "%d file" files))
										(and (> files 1) "s") "]")
									(lambda (&rest _)
										(require 'gptel-context)
										(gptel-context--buffer-setup)))
								'mouse-face 'highlight
								'help-echo "Active gptel context"))))
				(toggle-track-media
					(lambda (&rest _)
						(setq-local gptel-track-media (not gptel-track-media))
						(if gptel-track-media
							(progn
								(run-hooks 'gptel-refresh-buffer-hook)
								(message "Sending media from included links."))
							(without-restriction (gptel--annotate-link-clear))
							(message "Ignoring links.  Only link text will be sent."))
						(run-at-time 0 nil #'force-mode-line-update)))
				(track-media
					(and (gptel--model-capable-p 'media)
						(if gptel-track-media
							(propertize
								(buttonize "[Media: Send]" toggle-track-media)
								'mouse-face 'highlight
								'help-echo
								"Sending media from links/urls when supported.\nClick to toggle")
							(propertize
								(buttonize "[Media: No]" toggle-track-media)
								'mouse-face 'highlight
								'help-echo
								"Ignoring media from links/urls.\nClick to toggle"))))
				(toggle-tools (lambda (&rest _) (interactive)
								(run-at-time 0 nil
									(lambda () (call-interactively #'gptel-tools)))))
				(tools (when (and gptel-use-tools gptel-tools)
						(propertize
							(buttonize (pcase (length gptel-tools)
										(0 "[No tools]") (1 "[1 tool]")
										(len (format "[%d tools]" len)))
								toggle-tools)
							'mouse-face 'highlight
							'help-echo "Select tools"))))
			(let ((rhs (concat
						usage (and usage " ")
						cost (and cost " ")
						tools (and tools " ")
						track-media (and track-media " ")
						context (and context " ")
						preset " "
						(propertize
							(buttonize (concat "[" model "]")
								(lambda (&rest _) (gptel-menu)))
							'mouse-face 'highlight
							'help-echo "Model in use"))))
				(concat
					(propertize
						" " 'display
						(if (and (fboundp 'string-pixel-width)
								(display-graphic-p))
							`(space :align-to (- right (,(string-pixel-width rhs))))
							`(space :align-to (- right ,(+ 5 (string-width rhs))))))
					rhs))))
	"Header-line info segment for gptel buffers.
Replaces `gptel--header-line-info' set by the function `gptel-use-header-line'.")

(defun joes-ai-gptel--use-header-line (&rest _)
	"Replace the info segment of the gptel header line."
	(setf (nth 2 header-line-format) joes-ai-gptel--header-line-info))

(advice-add #'gptel-use-header-line :after #'joes-ai-gptel--use-header-line)

(advice-add 'gptel--update-token-usage :before #'joes-ai-gptel--record-turn-usage)

(provide 'joes-ai-headerline)
;;; joes-ai-headerline.el ends here
