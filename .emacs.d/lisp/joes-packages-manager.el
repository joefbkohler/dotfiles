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

(defun my-register-fork-packages()
	"Register my forks so correct packages are installer."
	(straight-register-package
		'(tree-sitter-indent :fork "ssh://git@codeberg.org/joejunior/tree-sitter-indent.el"))
	(straight-register-package '(csharp-mode :fork "joefbsjr/csharp-mode")))

(defun my-install-default-packages()
	"Install permanent packages."
	(straight-use-package 'ivy-xref :no-clone t)
	(straight-use-package 'tree-sitter :no-clone t)
	(straight-use-package 'go-mode :no-clone t)
	(straight-use-package 'smex :no-clone t)
	(straight-use-package 'lsp-ivy :no-clone t)
	(straight-use-package 'counsel :no-clone t)
	(straight-use-package 'magit-lfs :no-clone t)
	(straight-use-package 'magit :no-clone t)
	(straight-use-package 'yasnippet :no-clone t)
	(straight-use-package 'pdf-tools :no-clone t)
	(straight-use-package 'lsp-latex :no-clone t)
	(straight-use-package 'vue-mode :no-clone t)
	(straight-use-package 'lsp-ui :no-clone t)
	(straight-use-package 'jedi :no-clone t)
	(straight-use-package 'highlight-indent-guides :no-clone t)
	(straight-use-package 'pyvenv :no-clone t)
	(straight-use-package 'yaml-mode :no-clone t)
	(straight-use-package 'json-mode :no-clone t)
	(straight-use-package 'dockerfile-mode :no-clone t)
	(straight-use-package 'typescript-mode :no-clone t)
	(straight-use-package 'eglot :no-clone t)
	(straight-use-package 'lsp-mode :no-clone t)
	(straight-use-package 'jupyter :no-clone t)
	(straight-use-package 'gnu-elpa-keyring-update :no-clone t)
	(straight-use-package 'ivy :no-clone t)
	(straight-use-package 'exwm :no-clone t)
	(straight-use-package 'smartparens :no-clone t)
	(straight-use-package 'adaptive-wrap :no-clone t)
	(straight-use-package 'zenburn-theme :no-clone t)
	(straight-use-package 'logview :no-clone t)
	(straight-use-package 'flycheck :no-clone t)
	(straight-use-package 'csharp-mode))

(provide 'joes-packages-manager)
;;; joes-packages-manager.el ends here
