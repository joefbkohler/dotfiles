;;; Theme --- My custom theme configuration
;;; Commentary:
;;; Code:
(defun apply-zenburn-theme()
	(load-theme 'zenburn t)
	(set-face-attribute 'default nil :background "#181818" :height 180 :font "-UKWN-Victor Mono-semibold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
	(set-face-attribute 'hl-line nil :background "#111" :box '(:line-width -1 :color "#555"))
	(set-face-attribute 'region nil :foreground 'unspecified :background "#334")
	(set-face-attribute 'font-lock-constant-face nil :foreground "#F0DFAF" :weight 'bold)
	(set-face-attribute 'font-lock-builtin-face nil :foreground "#A5A5A5" :weight 'bold)
	(set-face-attribute 'font-lock-string-face nil :italic t))

(defun apply-tree-sitter-theme()
	(tree-sitter-hl-add-patterns 'c-sharp [(variable_declarator (identifier) @variable.parameter)])
	
	(set-face-attribute 'tree-sitter-hl-face:type.parameter nil :foreground "#90649d")
	(set-face-attribute 'tree-sitter-hl-face:variable.parameter nil :inherit 'font-lock-variable-name-face)
	(set-face-attribute 'tree-sitter-hl-face:method.call nil :inherit 'font-lock-function-name-face)
	(set-face-attribute 'tree-sitter-hl-face:variable nil :inherit 'default))

(defun apply-company-theme()
	(set-face-attribute 'company-tooltip-annotation nil :background 'unspecified))

(defun apply-flycheck-theme()
	(set-face-attribute 'flycheck-error nil :underline '(:color "#F00" :style wave)))

(defun apply-logview-theme()
	(require 'logview)
	(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
	(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
	(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
	(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified))

(defun apply-cursor-theme()
	(add-to-list 'default-frame-alist '(mouse-color . "#cca"))
	(add-to-list 'default-frame-alist '(cursor-color . "#cca")))

(provide 'joes-theme)
;;; joes-theme.el ends here
