;;; joes-theme.el --- Theme config for everything    -*- lexical-binding: t; -*-

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

(defface documents-face '((t :font "Iosevka Etoile"))
	"Face for text documents.")

(defun joes-theme-apply-flymake()
	(set-face-attribute 'flymake-error nil :underline '(:color "#F00" :style wave))
	(set-face-attribute 'flymake-warning nil :underline '(:color "#FF0" :style wave))
	(set-face-attribute 'flymake-note nil :underline '(:color "#00F" :style wave))
	(setq-default flymake-note-bitmap '(right-arrow compilation-note))
	(setq-default flymake-warning-bitmap '(right-triangle compilation-warning))
	(setq-default flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error)))

(defun joes-theme-apply-default-faces ()
	(setq-default visual-line-fringe-indicators 'left-curly-arrow right-curly-arrow)
	(setq-default buffer-face-mode-face 'documents-face)
	(set-face-attribute 'default nil
						:height 160
						:width 'normal
						:font "Iosevka Fixed")
	(set-face-attribute 'font-lock-string-face nil
						:inherit 'default
						:slant 'italic))

(defun joes-theme-apply-zenburn()
	(load-theme 'zenburn t)
	(joes-theme-apply-default-faces)
	(set-face-attribute 'default nil
		:background "#111"
		:foreground "#D6D0BC")
	(set-face-attribute 'hl-line nil :background "#0A0A0A" :box '(:line-width -1 :color "#555"))
	(set-face-attribute 'region nil :foreground 'unspecified :background "#18181F")
	(set-face-attribute 'isearch nil :foreground 'unspecified)
	(set-face-attribute 'lazy-highlight nil :foreground 'unspecified)

	(set-face-attribute 'font-lock-keyword-face nil :foreground "#AA6" :weight 'bold)
	(set-face-attribute 'font-lock-constant-face nil :inherit 'font-lock-keyword-face :foreground "#C66")
	(set-face-attribute 'font-lock-builtin-face nil :foreground "#A5A5A5" :weight 'bold)

	(set-face-attribute 'font-lock-type-face nil :foreground "#6292A8")
	(set-face-attribute 'line-number nil :background "#151515")
	(set-face-attribute 'line-number-current-line nil :background "#111" :box '(:line-width -1 :color "#555"))

	(set-face-attribute 'highlight nil :background 'unspecified :weight 'ultra-bold :underline "#FFF"))

(defun joes-theme-apply-ivy()
	(require 'ivy-faces)
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
	(set-face-attribute 'ivy-minibuffer-match-face-4 nil :background 'unspecified :underline "#FFF"))

(defun joes-theme-apply-logview()
	(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
	(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
	(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
	(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified))

(defun joes-theme-set-cursor()
	(add-to-list 'default-frame-alist '(mouse-color . "#cca"))
	(add-to-list 'default-frame-alist '(cursor-color . "#cca")))

(defun joes-theme-set-ligatures ()
	"Set list of ligatures for each mode."
	(declare-function ligature-set-ligatures "ligature")
	(ligature-set-ligatures
		'prog-mode
		'("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
			 ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
			 "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
			 "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
			 "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
			 "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
			 "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
			 "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
			 ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
			 "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
			 "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
			 "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
			 "\\\\" "://")))

(provide 'joes-theme)
;;; joes-theme.el ends here
