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
;;; TODO: Write directives per mode (start with git commit)
;;; TODO: Skill system.	 Add dynamic directive that can fetch skills
;;; from list.	Create tool to fetch skills from a file and add to skills list.
;;; TODO: Create/add tools to modify files and other Emacs functionality.
;;; Code:

(require 'minuet)
(require 'gptel)
(require 'gptel-openai)
(require 'joes-keybindings)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-ai-chat-host "localhost:11434"
	"Chat models host address with port."
	:type 'string)

(defcustom joes-ai-completion-host "localhost:11434"
	"Completion models host address with port."
	:type 'string)

(defcustom joes-ai-completion-model "qwen2.5-coder"
	"Completion model.	Ideally a light and quick model.
Must have openai compatible FIM support."
	:type 'string)

(defcustom joes-ai-chat-models '((qwen3.5
									 :capabilities (media tool-use json url)
									 :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp" "application/pdf"))
									qwen2.5-coder)
	"Chat models.  Models for more complex tasks."
	:type '(list (string)))

(defun joes-init-ai ()
	"Initialize chat and completion models.	 Depends on llm servers being up."
	(interactive)
	(let ((host
			  "http://%s/v1/chat/completions")
			 (body "{\"reset\": true,\"messages\":[{\"role\":\"user\",\"content\":\"\"}],\"model\": \"%s\"}"))
		(dolist (model joes-ai-chat-models)
			(let ((name (symbol-name (if (consp model) (car model) model))))
				(plz 'post
					(format host joes-ai-chat-host)
					:body (format body name)
					:then (lambda (_) (message "%s:%s" "Loaded Chat Model" name)))))
		(plz 'post
			(format host joes-ai-completion-host)
			:body (format body joes-ai-completion-model)
			:then (lambda (_) (message "%s" "Loaded Completion Model")))))

;; --- GPTEL configuration ---

(setq gptel-default-mode #'org-mode)
(setq gptel-expert-commands t)
(setq-default gptel-model
	(let ((model (car joes-ai-chat-models))) ;; Default to the first model
		(if (consp model)					 ;; Check if the model is a list eg. (qwen3.5 . capabilities)
			(car model)
			model)))

(gptel-make-openai "ai-chat"
	:stream t
	:protocol "http"
	:host joes-ai-chat-host
	:models joes-ai-chat-models
	:request-params '(:chat_template_kwargs (:enable_thinking :json-false)))

(setq gptel-backend
	(gptel-make-openai "ai-thinking"
		:stream t
		:protocol "http"
		:host joes-ai-chat-host
		:models joes-ai-chat-models
		:request-params '(:chat_template_kwargs nil)))

(defun joes-gptel-magit-commit-context ()
	"Add `magit-diff' buffer to `gptel-context' locally."
	(declare-function magit-get-mode-buffer "magit")
	(setq-local gptel-context (list (magit-get-mode-buffer 'magit-diff-mode)))
	(setq-local gptel-backend (gptel-get-backend "ai-chat")))

(with-eval-after-load 'magit
	(add-hook 'git-commit-setup-hook #'joes-gptel-magit-commit-context))

;; --- Minuet configuration ---

(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 1)
(setq minuet-context-window 16500)
(setq minuet-request-timeout 3)
(setq minuet-auto-suggestion-debounce-delay 0.2)
(setq minuet-auto-suggestion-throttle-delay 0.5)
(setq minuet-after-cursor-filter-length 1)

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
	"Check if auto-suggestion should be enabled.
Disable if too many characters in buffer."
	(when (and minuet-auto-suggestion-mode
			  (> (buffer-size) minuet-context-window))
		(minuet-auto-suggestion-mode -1)))

(add-to-list 'minuet-auto-suggestion-block-predicates #'joes-ai-minuet-check-if-insert-command-p)
(add-hook 'minuet-auto-suggestion-mode-hook #'joes-ai-minuet-check-auto-suggestion)

;; --- AI end ---

(joes-keybinding-ai)

(provide 'joes-ai)

;;; joes-ai.el ends here
