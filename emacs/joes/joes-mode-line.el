;;; joes-mode-line.el --- Mode-line layout and configuration  -*- lexical-binding: t; -*-

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

(require 'vc-git)
(require 'joes-utils)
(require 'color)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defvar-local joes-mode-line-vc-branch nil
    "Branch of the current buffer repo with modified information.")

(defcustom joes-mode-line-area-divider " "
    "Divider from highlited areas to normal areas."
    :type 'string)

(defcustom joes-mode-line-area-divider-right " "
    "Divider from highlited areas to normal areas."
    :type 'string)

(defcustom joes-mode-line-vc-symbol ""
    "Symbol shown in modeline VC information."
    :type 'string)

(defcustom joes-mode-line-vc-modified-color "#A22"
    "Symbol shown in modeline VC information."
    :type 'string)

(defcustom joes-mode-line-highlight-color "#929"
    "Symbol shown in modeline VC information."
    :type 'string)

(defcustom joes-mode-line-color-delta 35
    "How much to change the previous section color to highlight new section."
    :type 'int)

(defcustom joes-mode-line-selected-window (frame-selected-window)
    "Selected window."
    :type 'int)

(defun joes-mode-line-update-window(_)
    "Update selected window."
    (setq joes-mode-line-selected-window (frame-selected-window))
    (force-mode-line-update))

(defun joes-mode-line-update-vc ()
	"Update VC branch and modified in mode-line."
	(setq joes-mode-line-vc-branch nil)
	(when (buffer-file-name) ;; short circuit to avoid unecessary git calls.
		(let ((branch (car (vc-git-branches))))
			(when branch ;; is inside a git repo.
				(joes-async-shell-command-to-string
					(lambda (result)
						(setq joes-mode-line-vc-branch
                            (format "%s%s%s"
								(propertize joes-mode-line-vc-symbol 'face
									(if (string-empty-p result)
                                        '(:foreground nil)
                                        `(:foreground ,joes-mode-line-vc-modified-color)))
								branch
                                (if (string-empty-p result) "" ""))))
					"git" "status" "--porcelain" "-z")))))


(defun joes-mode-line-colorize-recursive (section &optional color)
    "Add bg COLOR to all strings of a SECTION recursevely."
    (let ((color (or color joes-mode-line-highlight-color)))
        (if (proper-list-p section)
            (dolist (subsection section)
                (joes-mode-line-colorize-recursive subsection color))
            (when (stringp section)
                (let* ((current-face (get-text-property 0 'face section))
                          (face `(:background ,color))
                          (current-bg (car (cdr (assoc :background (ensure-list current-face))))))
                    (when (not (string= color current-bg))
                        (add-face-text-property 0 (length section) face nil section))
					)
                )))
    section)

(defun joes-mode-line-colored-divider(divider new-color &optional last-color)
    "Return DIVIDER with the correct NEW-COLOR comming from LAST-COLOR."

    (let* ((current-bg (or last-color (joes-mode-line-current-background))))
        `(2 ,(propertize divider
                 'face
                 `(:background ,new-color :foreground ,current-bg)))))

(defun joes-mode-line-colorized-section (section divider depth &optional new-color)
    "Add color to SECTION, a DIVIDER before it.
DEPTH help find the correct color by darkening it a certain amount.
NEW-COLOR can be set to override the color selected."
    (let ((new-color (or new-color
                         (color-darken-name joes-mode-line-highlight-color
                             (* depth joes-mode-line-color-delta))))
             (last-color (if (< (- depth 1) 0)
                             (joes-mode-line-current-background)
                             (color-darken-name joes-mode-line-highlight-color
                                 (* (- depth 1) joes-mode-line-color-delta)))))
        (remove 'nil
            (list
                (when (eq divider joes-mode-line-area-divider)
                    (joes-mode-line-colored-divider divider new-color last-color))
                (joes-mode-line-colorize-recursive section new-color)
                (when (eq divider joes-mode-line-area-divider-right)
                    (joes-mode-line-colored-divider divider new-color last-color))))))

(defun joes-mode-line-current-background ()
    "Background color based on the mode-line being active or not."
    (if (eq (selected-window) joes-mode-line-selected-window)
        (face-background 'mode-line)
        (face-background 'mode-line-inactive)))

(setq-default mode-line-format
	'((:eval (list
                 mode-line-front-space
                 `((2 ,(if buffer-read-only "󰷪" "󰲶"))
				      (2 ,(if (file-remote-p default-directory) "󰲁" "󰉖"))
				      (2 ,(if (buffer-modified-p) "󰷈" "󱪚")))
                 (joes-mode-line-colorized-section
                     (list
                         (format-mode-line mode-line-buffer-identification)
                         (when mode-line-process mode-line-process))
                     joes-mode-line-area-divider 0)
                 (joes-mode-line-colorized-section
                     (when joes-mode-line-vc-branch joes-mode-line-vc-branch)
                     joes-mode-line-area-divider 1)
                 (joes-mode-line-colorized-section
                     (list (when (bound-and-true-p flymake-mode) (format-mode-line flymake-mode-line-format))
				         (when (not (string-empty-p (format-mode-line mode-line-misc-info))) (format-mode-line mode-line-misc-info)))
                     joes-mode-line-area-divider 2)
                 (joes-mode-line-colorized-section '("") joes-mode-line-area-divider 3 (joes-mode-line-current-background))))
         mode-line-format-right-align
		 (:eval (list
                    (joes-mode-line-colorized-section '("") joes-mode-line-area-divider-right 2 (joes-mode-line-current-background))
                    (joes-mode-line-colorized-section (format-mode-line mode-name) joes-mode-line-area-divider-right 1)
                    (joes-mode-line-colorized-section '("%3l%3C") joes-mode-line-area-divider-right 0)
                    '((-3 "%p") (1 " "))))
         mode-line-end-spaces
         ))

(provide 'joes-mode-line)
;;; joes-mode-line.el ends here
