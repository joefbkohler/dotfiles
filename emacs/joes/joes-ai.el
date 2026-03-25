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
(require 'gptel-ollama)
(require 'joes-keybindings)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-ollama-reasoning-host "localhost:11434"
	"Ollama for reasoning models host address with port."
	:type 'string)

(defcustom joes-ollama-completion-host "localhost:11434"
	"Ollama for completion models host address with port."
	:type 'string)

(defcustom joes-ollama-completion-model "qwen2.5-coder:1.5b"
	"Ollama completion model.  Ideally a light and quick model.
Must have openai compatible FIM support."
	:type 'string)

(defcustom joes-ollama-reasoning-models '(qwen3:8b-16k)
	"Ollama reasoning model.  Model for more complex tasks."
	:type 'list)

(defun joes-init-ai()
	"Try to start ai models.  Depends on Ollama servers being up."
	(interactive)
	(async-shell-command (concat "curl http://"
							 joes-ollama-reasoning-host
							 "/api/generate -d '{\"model\": \"" (symbol-name (car joes-ollama-reasoning-models)) "\"}'")
		"Ollama Reasoning Load")
	(async-shell-command (concat "curl http://"
							 joes-ollama-completion-host
							 "/api/generate -d '{\"model\": \"" joes-ollama-completion-model "\"}'")
		"Ollama Completion Load "))

(defun joes-gptel-magit-commit-context ()
	"Add `magit-diff' buffer to `gptel-context' locally."
	(setq-local gptel-context (list (magit-get-mode-buffer 'magit-diff-mode))))

(setq gptel-use-context 'user)
(setq gptel-model (car joes-ollama-reasoning-models))
(setq gptel-backend (gptel-make-ollama "Ollama PC"
						:stream t
						:host joes-ollama-reasoning-host
						:models joes-ollama-reasoning-models
						:endpoint "/api/chat"))

(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 2)
(setq minuet-context-window 4096)
(setq minuet-request-timeout 3)

(plist-put minuet-openai-fim-compatible-options :api-key "TERM")
(plist-put minuet-openai-fim-compatible-options :name "Ollama")
(minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 50)
(minuet-set-optional-options minuet-openai-fim-compatible-options :temperature 0.2)
(minuet-set-optional-options minuet-openai-fim-compatible-options :stop ["\n" "<|endoftext|>"])

(plist-put minuet-openai-fim-compatible-options
	:end-point (concat "http://" joes-ollama-completion-host "/v1/completions"))
(plist-put minuet-openai-fim-compatible-options :model joes-ollama-completion-model)

(with-eval-after-load 'magit
	(add-hook 'git-commit-setup-hook #'joes-gptel-magit-commit-context))

(joes-keybinding-ai)

(provide 'joes-ai)
;;; joes-ai.el ends here
