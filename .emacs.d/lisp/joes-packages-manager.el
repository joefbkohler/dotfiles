;;; package --- Joe's Packages Manager
;;; Commentary:
;; My Package's configuration.  straight.el bootstrap
;; and packages installation
;;; Code:

(defvar bootstrap-version)

(defun my-straight-initialize ()
	"Initialize straight.el."
	(let ((bootstrap-file
			  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
			 (bootstrap-version 5))
		(unless (file-exists-p bootstrap-file)
			(with-current-buffer
				(url-retrieve-synchronously
					"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
					'silent 'inhibit-cookies)
				(goto-char (point-max))
				(eval-print-last-sexp)))
		(load bootstrap-file nil 'nomessage)))

(defun my-install-default-packages()
	"Install permanent packages."
	(straight-use-package 'ivy-xref)
	(straight-use-package 'tree-sitter)
	(straight-use-package 'go-mode)
	(straight-use-package 'smex)
	(straight-use-package 'lsp-ivy)
	(straight-use-package 'counsel)
	(straight-use-package 'magit-lfs)
	(straight-use-package 'company-box)
	(straight-use-package 'magit)
	(straight-use-package 'yasnippet)
	(straight-use-package 'pdf-tools)
	(straight-use-package 'lsp-latex)
	(straight-use-package 'vue-mode)
	(straight-use-package 'lsp-ui)
	(straight-use-package 'jedi)
	(straight-use-package 'highlight-indent-guides)
	(straight-use-package 'pyvenv)
	(straight-use-package 'yaml-mode)
	(straight-use-package 'json-mode)
	(straight-use-package 'dockerfile-mode)
	(straight-use-package 'typescript-mode)
	(straight-use-package 'eglot)
	(straight-use-package 'lsp-mode)
	(straight-use-package 'jupyter)
	(straight-use-package 'gnu-elpa-keyring-update)
	(straight-use-package 'ivy)
	(straight-use-package 'exwm)
	(straight-use-package 'smartparens)
	(straight-use-package 'adaptive-wrap)
	(straight-use-package 'zenburn-theme)
	(straight-use-package 'logview)
	(straight-use-package 'company)
	(straight-use-package 'flycheck)
	(straight-use-package
		'(csharp-mode :type git :host github :repo "emacs-csharp/csharp-mode"
             :fork (:host github
					   :repo "joefbsjr/csharp-mode")))
	(straight-use-package
		'(tree-sitter-indent :type git :repo "ssh://git@codeberg.org/FelipeLema/tree-sitter-indent.el.git"
             :fork (:repo "ssh://git@codeberg.org/joejunior/tree-sitter-indent.el"))))

(provide 'joes-packages-manager)
;;; joes-packages-manager.el ends here
