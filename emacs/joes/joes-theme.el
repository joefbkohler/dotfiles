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

(require 'hl-line)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-use-transparency t
	"Should use transparency in background."
	:type 'boolean)

(defface documents-face '((t :font "Iosevka Etoile"))
	"Face for text documents.")

(defun joes-theme-apply-default-faces ()
	"Default face."
	(setq-default visual-line-fringe-indicators 'left-curly-arrow right-curly-arrow)
	(setq-default buffer-face-mode-face 'documents-face) ;; For documents buffers.
	(set-face-attribute 'default nil
		:height 160
		:width 'normal
		:font "Iosevka")
	(set-face-attribute 'font-lock-string-face nil
		:inherit 'default
		:slant 'italic)
    (set-face-attribute 'mode-line nil
        :font "Iosevka Nerd Font")
    (set-face-attribute 'mode-line-inactive nil
        :inherit 'mode-line))

(defun joes-theme-apply-flymake()
	"Flymake theme config."
	(set-face-attribute 'flymake-error nil :underline '(:color "#F00" :style wave))
	(set-face-attribute 'flymake-warning nil :underline '(:color "#FF0" :style wave))
	(set-face-attribute 'flymake-note nil :underline '(:color "#00F" :style wave))
	(setq-default flymake-note-bitmap '(right-arrow compilation-note))
	(setq-default flymake-warning-bitmap '(right-triangle compilation-warning))
	(setq-default flymake-error-bitmap '(flymake-double-exclamation-mark compilation-error)))

(defun joes-theme-apply-zenburn()
	"General colors based on zenburn."
	(load-theme 'zenburn t)
	(joes-theme-apply-default-faces)
	(set-face-attribute 'default nil
		:background "#111"
		:foreground "#D6D0BC")
	
    (set-face-attribute 'hl-line nil :background "#0D0D0D" :box '(:line-width -1 :color "#555"))
	(set-face-attribute 'region nil :foreground 'unspecified :background "#322")
	(set-face-attribute 'isearch nil :foreground 'unspecified)
	(set-face-attribute 'lazy-highlight nil :foreground 'unspecified)
	
	(set-face-attribute 'fringe nil :foreground 'unspecified :background "#222")

	(set-face-attribute 'mode-line nil :background "#161616" :height 140)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line :background "#262626")

	(set-face-attribute 'font-lock-keyword-face nil :foreground "#AA6" :weight 'bold)
	(set-face-attribute 'font-lock-constant-face nil :inherit 'font-lock-keyword-face :foreground "#C66")
	(set-face-attribute 'font-lock-builtin-face nil :foreground "#A5A5A5" :weight 'bold)

	(set-face-attribute 'font-lock-type-face nil :foreground "#6292A8")
	(set-face-attribute 'line-number nil :background "#141414")
	(set-face-attribute 'line-number-current-line nil :background "#111" :box '(:line-width -1 :color "#555"))

	(set-face-attribute 'highlight nil :background 'unspecified :weight 'ultra-bold :underline "#FFF")

    (set-frame-parameter (selected-frame) 'alpha-background 100)
    (set-frame-parameter (selected-frame) 'alpha '(100 100)))

(defun joes-theme-darker-transparent-background()
    "Darker background colors for use with transparency."
    (set-face-attribute 'default nil :background "#000")
    (set-face-attribute 'hl-line nil :background "#080808")
    (set-face-attribute 'fringe nil :background "#111")
    (set-face-attribute 'region nil :background "#281818")
    (set-face-attribute 'mode-line nil :background "#060606")
    (set-face-attribute 'mode-line-inactive nil :background "#161616")
    (set-face-attribute 'line-number nil :background "#060606")
	(set-face-attribute 'line-number-current-line nil :background "#000")
    (set-frame-parameter (selected-frame) 'alpha-background 90)
    (set-frame-parameter (selected-frame) 'alpha '(100 90)))

(defun joes-theme-apply-ivy()
	"Ivy/Swiper/Counsel colors."
	(require 'ivy-faces)
	(set-face-attribute 'ivy-current-match nil
		:background "#0A0E0A"
		:box '(:line-width -1 :color "#606560")
		:foreground 'unspecified
		:underline 'unspecified
		:extend t)
	(set-face-attribute 'ivy-subdir nil :background 'unspecified :foreground 'unspecified :inherit 'dired-directory)
	(set-face-attribute 'ivy-minibuffer-match-face-1 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-2 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-3 nil :background 'unspecified :underline "#FFF")
	(set-face-attribute 'ivy-minibuffer-match-face-4 nil :background 'unspecified :underline "#FFF"))

(defun joes-theme-apply-logview()
	"Logview theme."
	(set-face-attribute 'logview-warning-entry nil :foreground "#B90" :background 'unspecified)
	(set-face-attribute 'logview-error-entry nil :foreground "#A33" :background 'unspecified)
	(set-face-attribute 'logview-trace-entry nil :foreground "#AAC" :background 'unspecified)
	(set-face-attribute 'logview-information-entry nil :foreground "#3B3" :background 'unspecified))

(defun joes-theme-apply-magit()
    "Magit colors."
    (set-face-attribute 'magit-section-highlight nil :background "#262626")
    (set-face-attribute 'magit-diff-hunk-heading-highlight nil :background "#262626")
    (set-face-attribute 'magit-diff-context-highlight nil :background "#1F1F1F")
    (set-face-attribute 'magit-diff-removed nil :background "#5C1313")
    (set-face-attribute 'magit-diff-added nil :background "#3F5F3F")
    (set-face-attribute 'magit-diff-removed-highlight nil :background "#6C2323")
    (set-face-attribute 'magit-diff-added-highlight nil :background "#4F6F4F"))

(defun joes-theme-set-cursor()
	"Mouse cursor."
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

(joes-theme-apply-default-faces)

(provide 'joes-theme)
;;; joes-theme.el ends here
