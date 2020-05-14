;;; package --- ligature-mode
;;; Commentary:
;;; Turns on ligature.  Currently uses fira-code-mode.
;;; Code:

(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
     (while (< n width)
       (setq prefix (append prefix '(?\s (Br . Bl))))
       (setq n (1+ n)))
     (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
	'("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
		 "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
		 "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
		 "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
		 ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
		 "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
		 "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
		 "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
		 ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
		 "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
		 "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
		 "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
		 "x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defvar-local ligature-mode--exception)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode ligature-mode
  "Fira Code ligatures minor mode"
  :lighter " ligature"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if ligature-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'ligature-mode)
;;; ligature-mode ends here
