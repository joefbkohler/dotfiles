;;; Theme --- My custom theme configuration
;;; Commentary:
;;; Code:

(defgroup joe nil
	"My little modifications"
	:group 'convenience)

(defcustom ivy-switch-buffer-major-mode-column 35
	"Column where the Major Mode of the buffer should show in `ivy-switch-buffer'."
	:type 'integer)
(defcustom ivy-switch-buffer-path-column 60
	"Column where the Path of the file visited by the buffer should show in `ivy-switch-buffer'."
	:type 'integer)
(defcustom ivy-counsel-doc-column 40
	"Column where the documentation should show in `counsel'."
	:type 'integer)

(defun apply-flymake-theme()
	(require 'flymake)
	(set-face-attribute 'flymake-error nil :underline '(:color "#F00" :style wave))
	(set-face-attribute 'flymake-warning nil :underline '(:color "#FF0" :style wave))
	(set-face-attribute 'flymake-note nil :underline '(:color "#00F" :style wave))
	(setq-default flymake-note-bitmap '(right-arrow compilation-note))
	(setq-default flymake-warning-bitmap '(right-triangle compilation-warning))
	(setq-default flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error)))

(defun apply-zenburn-theme()
	(require 'linum)
	(load-theme 'zenburn t)
	(set-face-attribute 'default nil
		:background "#181818"
		:foreground "#D6D0BC"
		:height 180
		:font "Iosevka Fixed")
	(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -1 :color "#555"))
	(set-face-attribute 'region nil :foreground 'unspecified :background "#334")
	(set-face-attribute 'isearch nil :foreground 'unspecified)
	(set-face-attribute 'lazy-highlight nil :foreground 'unspecified)

	(set-face-attribute 'font-lock-keyword-face nil :foreground "#AA6" :weight 'bold)
	(set-face-attribute 'font-lock-constant-face nil :inherit 'font-lock-keyword-face :foreground "#C66")
	(set-face-attribute 'font-lock-builtin-face nil :foreground "#A5A5A5" :weight 'bold)
	(set-face-attribute 'font-lock-string-face nil
		:font "-UKWN-Victor Mono-semibold-normal-normal-*-*-*-*-*-m-0-iso10646-1"
		:italic t)
	(set-face-attribute 'font-lock-type-face nil :foreground "#6292A8")

	(set-face-attribute 'linum nil :foreground "#777" :height (face-attribute 'default :height))
	(set-face-attribute 'highlight nil :background 'unspecified :weight 'ultra-bold :underline "#FFF"))

(defun apply-tree-sitter-theme()
	(require 'tree-sitter-hl)
	(set-face-attribute 'tree-sitter-hl-face:type.parameter nil :foreground "#90649D")
	(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil :inherit 'font-lock-variable-name-face)
	(set-face-attribute 'tree-sitter-hl-face:property nil :inherit 'font-lock-variable-name-face :italic nil)
	(set-face-attribute 'tree-sitter-hl-face:method.call nil :inherit 'font-lock-function-name-face)
	(set-face-attribute 'tree-sitter-hl-face:method nil :inherit 'font-lock-function-name-face :italic nil)
	(set-face-attribute 'tree-sitter-hl-face:variable nil :inherit 'default))

(defun apply-ivy-theme()
	(set-face-attribute 'ivy-current-match nil
		:background "#223"
		:box '(:line-width -1 :color "#080")
		:foreground 'unspecified
		:underline 'unspecified
		:extend t)
	(set-face-attribute 'ivy-subdir nil :background 'unspecified :foreground 'unspecified :inherit 'dired-directory)
	(set-face-attribute 'ivy-minibuffer-match-face-1 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-2 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-3 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-4 nil :background 'unspecified :underline "#FFF")

	;; transformers
	(ivy-configure 'ivy-switch-buffer :display-transformer-fn 'ivy-switch-buffer-mode-path-transformer)
	(ivy-configure 'counsel-M-x :display-transformer-fn 'ivy-counsel-function-doc-transformer)
	(ivy-configure 'counsel-describe-variable :display-transformer-fn 'ivy-counsel-variable-doc-transformer)
	(ivy-configure 'counsel-describe-function :display-transformer-fn 'ivy-counsel-function-doc-transformer)
	(ivy-configure t :format-fn 'ivy-format-function-line))

(defun apply-logview-theme()
	(require 'logview)
	(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
	(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
	(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
	(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified))

(defun apply-cursor-theme()
	(add-to-list 'default-frame-alist '(mouse-color . "#cca"))
	(add-to-list 'default-frame-alist '(cursor-color . "#cca")))

;; Ivy prettify ;)
(defun ivy-switch-buffer-mode-path-transformer (buffer-name)
	"Transformer for `ivy-switch-buffer' that add major mode and path for BUFFER-NAME."
	(let ((buffer (get-buffer buffer-name))
			 (result buffer-name))
		(if buffer
			(let ((buffer-major-mode (string-remove-suffix "-mode" (symbol-name (with-current-buffer buffer major-mode))))
					 (buffer-path (buffer-file-name buffer))
					 (name-size (string-width result)))
				(let ((result (concat result
								  (make-string (max 1 (- ivy-switch-buffer-major-mode-column name-size)) ? )
								  (propertize (concat "" buffer-major-mode) 'face 'font-lock-type-face))))
					(let ((result (concat result
									  (make-string (max 1 (- ivy-switch-buffer-path-column (string-width result))) ? )
									  (propertize (concat "" buffer-path) 'face 'font-lock-comment-face))))
						(truncate-string-to-width result (frame-width) nil nil t t))))
			result)))

;; Ivy transformer functions
(defun ivy-counsel-doc-transformer (symbol-name docstring)
	"Transformer for ivy commands that add SYMBOL-NAME DOCSTRING."
	(truncate-string-to-width
		(let ((result
				  (car (split-string
						   (concat
							   symbol-name
							   (make-string (max 1 (- ivy-counsel-doc-column (string-width symbol-name))) ? )
							   (propertize docstring 'face 'font-lock-comment-face))
						   "\n"))))
				result)
		(frame-width) nil nil t t))

(defun ivy-counsel-function-doc-transformer (function-name)
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
		(ivy-counsel-doc-transformer name-with-keys
			(concat ""
				(when (functionp func)
					(documentation func))))))

(defun ivy-counsel-variable-doc-transformer (variable-name)
	"Transformer for ivy commands that add VARIABLE-NAME docstring."
	(ivy-counsel-doc-transformer variable-name
		(concat ""
			(documentation-property
				(car (read-from-string variable-name)) 'variable-documentation))))

(defun linum-format-function (num)
	(let* ((columns
			   (string-width
				   (concat "  " (pp-to-string (count-lines (point-min) (point-max))))))
			  (num-str (pp-to-string num))
			  (num-width (string-width num-str))
			  (left-space-width (- columns num-width 1)))
		(propertize
			(concat (make-string left-space-width ? )  num-str " ")	'face 'linum)))

(provide 'joes-theme)
;;; joes-theme.el ends here
