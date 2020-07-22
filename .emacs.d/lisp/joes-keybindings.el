;;; Keybindings --- custom keybindings
;;; Commentary:
;;; Code:

(defun simulate-command-key()
	(global-set-key (kbd "s-v") 'yank)
	(global-set-key (kbd "s-x") 'kill-region)
	(global-set-key (kbd "s-c") 'kill-ring-save)
	(global-set-key (kbd "s-u") 'revert-buffer))

(defun set-common-keybindings()
	;; Unset some keybindings that I hate!
	(global-unset-key (kbd "C-v"))
	(global-unset-key (kbd "M-v"))
	
	(global-set-key (kbd "C-z") 'undo)
	(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up 1)))
	(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down 1)))
	(global-set-key (kbd "C-|") 'toggle-window-split)
	(global-set-key [remap shell-command] 'async-shell-command)
	
	;; Remove remapping of kill-line to kill-visual-line
	(define-key visual-line-mode-map [remap kill-line] nil)
	(global-set-key [remap isearch-forward] 'isearch-forward-regexp)
	(global-set-key [remap isearch-backward] 'isearch-backward-regexp)
	(global-set-key [remap isearch-forward-regexp] 'occur))

(defun set-ido-keybindings()
	(global-set-key (kbd "M-x") 'smex)
	(global-set-key [remap ido-toggle-prefix] 'ido-prev-match)
	(global-set-key [remap previous-history-element] 'ido-prev-match)
	(global-set-key [remap next-history-element] 'ido-next-match)
	(global-set-key [remap ido-next-work-directory] 'ido-next-match)
	(global-set-key [remap ido-prev-work-directory] 'ido-prev-match)
	(global-set-key [remap ido-complete-space] 'ido-restrict-to-matches))

(defun set-company-keybindings()
	(global-set-key [remap completion-at-point] 'company-complete))

(provide 'joes-keybindings)
;;; joes-keybindings.el ends here
