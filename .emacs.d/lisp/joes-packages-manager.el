;;; package --- Joe's Packages Manager
;;; Commentary:
;; My Package's configuration.
;; and packages installation
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-selected-packages
    '(ivy-xref
	     counsel
	     ivy-prescient
	     magit-lfs
	     magit
	     yasnippet
	     pdf-tools
	     jedi
	     highlight-indent-guides
	     pet
	     yaml-mode
	     json-mode
	     dockerfile-mode
	     jupyter
	     gnu-elpa-keyring-update
	     ivy
	     exwm
	     smartparens
	     adaptive-wrap
	     zenburn-theme
	     logview
	     csharp-mode
	     company
	     scad-mode
	     ligature
         tree-sitter
         tree-sitter-langs
         tree-sitter-indent
         consult
         exec-path-from-shell))

(defun my-register-fork-packages()
        (swiper :fork "joefbsjr/swiper" :branch "master"))

(provide 'joes-packages-manager)
;;; joes-packages-manager.el ends here
