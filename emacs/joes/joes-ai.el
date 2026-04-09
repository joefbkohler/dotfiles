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
;;; TODO: Skill system.	 Skills are presets.  Create tool to fetch available
;;; skills and apply preset.
;;; TODO: Create/add tools to modify files and other Emacs functionality.
;;; Code:

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

;; --- GPTEL configuration ---

(setopt gptel-directives
	'((default . "[General Instruction]
You are an efficient and professional reasoning assistant. Prioritize direct, accurate answers. Reason internally but be concise and focused. In your internal reasoning, avoid unnecessary step-by-step breakdowns, self-correction loops, or filler. Be decisive and get to the point quickly.")
		 (rewrite . "[Rewriting Instructions]
* Add no markdown or markup code fences.
* Output exclusivelly the new text, without any explanations or comments.
* Follow the instructions to rewrite the provided snippet.")
		 ))

(require 'gptel)
(require 'gptel-openai)
(require 'gptel-request)

(setq gptel-default-mode #'org-mode)
(setq gptel-expert-commands t)
(setq-default gptel-model
	(let ((model (car joes-ai-chat-models))) ;; Default to the first model
		(if (consp model)					 ;; Check if the model is a list eg. (qwen3.5 . capabilities)
			(car model)
			model)))

(setq gptel-backend
	(gptel-make-openai "ai-chat"
		:stream t
		:protocol "http"
		:host joes-ai-chat-host
		:models joes-ai-chat-models))

(gptel-make-preset 'disable-thinking
	:parents '(chat)
	:request-params '(:chat_template_kwargs (:enable_thinking :json-false))
	:system "[General Instruction]
You are an efficient and professional assistant. Prioritize direct, accurate answers. Avoid embelishing language and fillers.")

(gptel-make-preset 'programming
	:parents '(chat))

(gptel-make-preset 'chat
	:model 'qwen3.5)

(declare-function magit-get-mode-buffer "magit")
(gptel-make-preset 'commit-message
	:model 'qwen3.5
	:parents '(chat)
	:use-context 'system
	:include-reasoning nil
	:system '(:append "\n[Commit Message Instructions]
* Be concise and direct. Focus on the WHAT, not the WHY.
* Use precise language, avoid adjectives (like better, easier) and superlatives.
* Instead of descriptions like 'improved function', just describe what was done.
* Pay attention to whitespace and style changes.
* Answer with no markup guards nor explanation.
* Use `Conventional Commit' format:
```
<type>: <description>

[optional body]
```
* The description is a short summary of the code changes, e.g., fix: array parsing issue when multiple spaces were contained in string.
* Provide a longer commit body, if change is too complex to for a single line. If so, detail each change individually. Seperate each change with a blank line.
The types are:
feat – a new feature is introduced with the changes
fix – a bug fix has occurred
chore – changes that do not relate to a fix or feature and don't modify src or test files (for example updating dependencies)
refactor – refactored code that neither fixes a bug nor adds a feature
docs – updates to documentation such as a the README
style – changes that do not affect the meaning of the code, likely related to code formatting such as white-space, missing semi-colons, and so on.
test – including new or correcting previous tests
perf – performance improvements
ci – continuous integration related
build – changes that affect the build system or external dependencies
revert – reverts a previous commit"))

;; --- Minuet configuration ---

(require 'minuet)

(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 1)
(setq minuet-context-window 16500)
(setq minuet-request-timeout 3)
(setq minuet-auto-suggestion-debounce-delay 0.2)
(setq minuet-auto-suggestion-throttle-delay 0.5)
(setq minuet-after-cursor-filter-length 1)

(plist-put minuet-openai-fim-compatible-options :api-key "TERM")
(plist-put minuet-openai-fim-compatible-options :name "qwen")
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

(defun joes-ai-magit-gen-commit-message (&optional summary)
	"Commit message generator for Magit with SUMMARY."
	(interactive)
	(defvar git-commit-mode)
	(let* ((diff-buffer (magit-get-mode-buffer 'magit-diff-mode))
			  (diff-buffer-string (with-current-buffer diff-buffer
									  (buffer-substring-no-properties (point-min) (point-max))))
			  (summary (or summary (read-string "Summary: "))))
		(if (and diff-buffer git-commit-mode)
			(gptel-with-preset 'commit-message
				(let ((prompt (concat "Write a commit message for the diff:\n```" diff-buffer-string "\n```"
								  (when summary (format "\nCommit Summary: %s" summary)))))
					(gptel-request prompt :transforms gptel-prompt-transform-functions :stream t)))
			(error "No diff buffer or not in git-commit-mode"))))

(joes-keybinding-ai)

(provide 'joes-ai)

;;; joes-ai.el ends here
