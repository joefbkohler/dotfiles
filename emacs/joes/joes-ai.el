;;; joes-ai.el --- AI assistant configuration		 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Joe Köhler

;; Author: Joe Köhler <joe.fb.kohler@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'minuet)
(require 'gptel)
(require 'gptel-openai)
(require 'joes-keybindings)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-ai-reasoning-host "localhost:11434"
	"Reasoning models host address with port."
	:type 'string)

(defcustom joes-ai-completion-host "localhost:11434"
	"Completion models host address with port."
	:type 'string)

(defcustom joes-ai-completion-model "qwen2.5-coder"
	"Completion model.	Ideally a light and quick model.
Must have openai compatible FIM support."
	:type 'string)

(defcustom joes-ai-reasoning-models '(qwen3.5 qwen2.5-coder)
	"Reasoning model.  Model for more complex tasks."
	:type 'list)

(defun joes-init-ai()
	"Try to start models.  Depends on llm servers being up."
	(interactive)
	(async-shell-command (concat "curl http://"
							 joes-ai-reasoning-host
							 "/v1/chat/completions -d '{\"messages\":[{\"role\":\"user\",\"content\":\"\"}],\"model\": \"" (symbol-name (car joes-ai-reasoning-models)) "\"}'")
		"Reasoning Model Loaded")
	(async-shell-command (concat "curl http://"
							 joes-ai-completion-host
							 "/v1/chat/completions -d '{\"messages\":[{\"role\":\"user\",\"content\":\"\"}],\"model\": \"" joes-ai-completion-model "\"}'")
		"Completion Model Loaded "))

(setq gptel-model (car joes-ai-reasoning-models))

(setq gptel-backend (gptel-make-openai "llama-cpp"
						:stream t
						:protocol "http"
						:host joes-ai-reasoning-host
						:models joes-ai-reasoning-models))

(defun joes-gptel-magit-commit-context ()
	"Add `magit-diff' buffer to `gptel-context' locally."
	(declare-function magit-get-mode-buffer "magit")
	(setq-local gptel-context (list (magit-get-mode-buffer 'magit-diff-mode))))

(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 1)
(setq minuet-context-window 16500)
(setq minuet-request-timeout 3)
(setq minuet-auto-suggestion-debounce-delay 0.2)
(setq minuet-auto-suggestion-throttle-delay 0.5)

(plist-put minuet-openai-fim-compatible-options :api-key "TERM")
(plist-put minuet-openai-fim-compatible-options :name "qwen")
(plist-put minuet-openai-fim-compatible-options :n 3)
(minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 100)
(minuet-set-optional-options minuet-openai-fim-compatible-options :temperature 0.2)
(minuet-set-optional-options minuet-openai-fim-compatible-options :stop ["\n"])

(plist-put minuet-openai-fim-compatible-options
	:end-point (concat "http://" joes-ai-completion-host "/v1/completions"))
(plist-put minuet-openai-fim-compatible-options :model joes-ai-completion-model)

;; Minuet auto suggestion configuration
(defun joes-ai-minuet-check-if-insert-command-p ()
	"Check if the current command is an insert command."
	(not (eq last-command 'self-insert-command)))

(defun joes-ai-minuet-check-auto-suggestion ()
	"Check if auto-suggestion should be enabled."
	(when (and minuet-auto-suggestion-mode
			  (> (buffer-size) minuet-context-window))
		(minuet-auto-suggestion-mode -1)))

(add-to-list 'minuet-auto-suggestion-block-predicates #'joes-ai-minuet-check-if-insert-command-p)
(add-hook 'minuet-auto-suggestion-mode-hook #'joes-ai-minuet-check-auto-suggestion)

(with-eval-after-load 'magit
	(add-hook 'git-commit-setup-hook #'joes-gptel-magit-commit-context))

(joes-keybinding-ai)

(provide 'joes-ai)

;;; joes-ai.el ends here
