;;; package --- Joe's Packages Manager
;;; Commentary:
;; My Package's configuration.  straight.el bootstrap
;; and packages installation
;;; Code:

(defvar bootstrap-version)

(defun my-install-default-packages()
	"Install permanent packages."
	(straight-use-package 'ivy-xref)
	(straight-use-package 'tree-sitter)
	(straight-use-package 'tree-sitter-langs)
    (straight-use-package 'tree-sitter-indent)
	(straight-use-package 'lsp-ivy)
	(straight-use-package 'counsel)
	(straight-use-package 'ivy-prescient)
	(straight-use-package 'magit-lfs)
	(straight-use-package 'magit)
	(straight-use-package 'yasnippet)
	(straight-use-package 'pdf-tools)
	(straight-use-package 'lsp-latex)
	(straight-use-package 'jedi)
	(straight-use-package 'highlight-indent-guides)
	(straight-use-package 'pyvenv)
	(straight-use-package 'yaml-mode)
	(straight-use-package 'json-mode)
	(straight-use-package 'dockerfile-mode)
	(straight-use-package 'lsp-mode)
	(straight-use-package 'jupyter)
	(straight-use-package 'gnu-elpa-keyring-update)
	(straight-use-package 'ivy)
	(straight-use-package 'exwm)
	(straight-use-package 'smartparens)
	(straight-use-package 'adaptive-wrap)
	(straight-use-package 'zenburn-theme)
	(straight-use-package 'logview)
	(straight-use-package 'csharp-mode)
	(straight-use-package 'company-mode)
    (straight-use-package 'scad-mode)
    (straight-use-package 'dap-mode))

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
    "Register my forks so correct packages are installed."
    (straight-override-recipe
      '(swiper :fork "joefbsjr/swiper" :branch "fix-counsel-company"))
    )

(provide 'joes-packages-manager)
;;; joes-packages-manager.el ends here
