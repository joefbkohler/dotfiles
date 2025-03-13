;;; early-init.el --- Early init. Mainly path.       -*- lexical-binding: t; -*-

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

;; -- Paths --------- Set everything to follow XDG
(require 'xdg)
(require 'recentf)

(add-to-list 'load-path (expand-file-name "emacs/joes" (xdg-config-home)))
(add-to-list 'exec-path "/usr/local/bin")
(startup-redirect-eln-cache (expand-file-name "emacs/eln" (xdg-cache-home)))

(setopt recentf-save-file (expand-file-name "emacs/recentf" (xdg-state-home))
    custom-file (expand-file-name "emacs/emacs-custom.el" (xdg-state-home)))

(let ((autosave-dir (expand-file-name "emacs/autosaves/" (xdg-state-home))))
    (make-directory autosave-dir t)
    (setopt auto-save-list-file-prefix (expand-file-name ".saves-" autosave-dir)
        backup-directory-alist `((".*" . ,autosave-dir))
        auto-save-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat autosave-dir "\\1") t))))

(setopt lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" ,(concat "/var/tmp/" "\\1") t))
    package-user-dir (expand-file-name "emacs/elpa" (xdg-data-home))
    project-list-file (expand-file-name "emacs/projects" (xdg-state-home))

    prescient-save-file (expand-file-name "emacs/prescient-save.el" (xdg-state-home))
    transient-history-file (expand-file-name "emacs/history.el" (xdg-state-home))
    logview-cache-filename (expand-file-name "emacs/logview-cache.extmap" (xdg-cache-home))


;; Local envinronment configuration
(ignore-errors (load-file (expand-file-name "emacs/early-local.el" (xdg-config-home))))

(provide 'early-init)
;;; early-init.el ends here
