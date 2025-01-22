(require 'joes-utils)
(require 'joes-keybindings)

(defgroup joe nil
	"My little hooks"
	:group 'convenience)

(defcustom framework-path-override "/usr/lib/mono"
	"Path to .NETFramework"
	:type 'string)

(defun my-exwm-hook(switch)
	(require 'exwm)
	(require 'exwm-config)
	(set-exwm-keybindings)
	(exwm-config-default)
	(exwm-init)
	(set-common-exwm-simulation-keys)
	(ido-mode 0)
	(fringe-mode '(8 . 0))
	(display-time))

(defun my-ediff-mode-hook()
	(custom-set-variables '(ediff-split-window-function 'split-window-horizontally)))

(defun my-elisp-mode-hook ()
	(setq-local lisp-indent-offset (my-buffer-indentation-offset)))

(defun my-prog-mode-hook ()
	;; Look for a tab indentation, if found, set indent-tabs-mode
	(setq indent-tabs-mode (when (not (string-match
										  "^\s+[^[:blank:]]"
										  (buffer-substring-no-properties 1 (point-max)))) t))
	(flymake-mode 1)
	(display-line-numbers-mode 1)
	(setq tab-width 4)
	(setq-local company-dabbrev-ignore-case nil)
	(setq-local company-dabbrev-downcase nil))

(defun my-text-mode-hook ()
	(buffer-face-mode))

(defun my-latex-mode-hook ()
	(local-set-key [remap tex-compile] 'tex-compile-update)
	(add-to-list 'company-capf-prefix-functions 'my-latex-company-capf-prefix)
	(eglot-ensure)
	(flymake-mode 1)
	(set-fill-column 63)
	(auto-fill-mode 1))

(defun my-save-hook ()
	(delete-trailing-whitespace 0))

(defun my-python-mode-hook ()
	(setq indent-tabs-mode nil)
	(highlight-indent-guides-mode t)
	(pet-mode)
	(set-python-keybindings)
	(eglot-ensure))

(defun my-lsp-hook ()
	(yas-minor-mode)
	(set-lsp-keybinding))

(defun my-tree-sitter-mode-hook()
	(require 'tree-sitter-indent)
	(add-to-list 'company-capf-prefix-functions 'my-tree-sitter-company-capf-prefix)
	(tree-sitter-hl-mode)
	(when (boundp
			  (intern (format "tree-sitter-indent-%s-scopes"
						  (replace-regexp-in-string (rx "-mode") "" (symbol-name major-mode)))))
		(tree-sitter-indent-mode)))

(defun my-cpp-mode-hook ()
	(eglot-ensure))

(defun my-csharp-mode-hook ()
	(require 'whitespace)
	(require 'tree-sitter-langs)
	(setq-local csharp-indent-offset (my-buffer-indentation-offset))
	(setenv "FrameworkPathOverride" framework-path-override)
	(eglot-ensure)

	(tree-sitter-hl-add-patterns 'c-sharp
		[(variable_declarator (identifier) @variable.parameter)])

	(setq whitespace-style '(face trailing space-before-tab empty space-after-tab tab-mark))
	(whitespace-mode 1)
	(eldoc-mode -1))

(defun my-git-commit-mode-hook ()
	(setq-local company-dabbrev-ignore-case nil)
	(setq-local company-dabbrev-downcase nil)
	(set-git-commit-keybindings))

(provide 'joes-hooks)
;;; hooks.el ends here
