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
;;; TODO: Test prefill with local model.  If too bad, remove volatile information
;; from system, like last buffer, and make it a tool.
;;; Code:

(require 'joes-keybindings)

(defgroup joe nil
	"My little modifications."
	:group 'convenience)

(defcustom joes-ai-chat-host "localhost:11434"
	"Chat models host address with port."
	:type 'string)

(defcustom joes-ai-chat-protocol "http"
	"Chat models protocol."
	:type 'string)

(defcustom joes-ai-chat-endpoint "/v1/chat/completions"
	"Chat models API endpoint path."
	:type 'string)

(defcustom joes-ai-chat-key nil
	"Chat models API key.	Either a string, or a function returning one."
	:type '(choice (const nil) string function))

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

(defcustom joes-ai-commit-message-model nil
	"Model for the `commit-message' preset.	 nil uses the default `gptel-model'."
	:type '(choice (const :tag "Default chat model" nil) symbol))

(defcustom joes-ai-chat-reasoning-off
	'(:chat_template_kwargs (:enable_thinking :json-false))
	"Request params sent to disable or minimize chat model reasoning.
nil sends nothing."
	:type '(choice (const nil) plist))

(defcustom joes-ai-chat-reasoning-on
	'(:chat_template_kwargs (:enable_thinking t))
	"Request params sent to enable chat model reasoning.
nil sends nothing."
	:type '(choice (const nil) plist))

;; --- GPTEL configuration ---

(require 'gptel)
(require 'mcp-hub)
(require 'gptel-integrations)
(require 'gptel-openai)
(require 'gptel-request)
(require 'gptel-context)
(require 'joes-ai-directives)
(require 'joes-ai-headerline)

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
		:protocol joes-ai-chat-protocol
		:host joes-ai-chat-host
		:endpoint joes-ai-chat-endpoint
		:key joes-ai-chat-key
		:models joes-ai-chat-models))

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

;; --- Helper Functions ---

(defun joes-ai-preload-local-models ()
	"Initialize chat and completion models.	 Depends on llm servers being up."
	(interactive)
	(let ((host
			  (concat joes-ai-chat-protocol "://%s" joes-ai-chat-endpoint))
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
	(declare-function magit-get-mode-buffer "magit")
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

(defun joes-ai-add-project-dir-to-context ()
	"Add files with specific ext in dir to `gptel-context'."
	(interactive)
	(when-let* ((project (project-current))
				  (dir (read-directory-name "Choose directory to add: "))
				  (ext (read-string "File extension: "
						   (file-name-extension (buffer-file-name)))))
		(declare-function project-files "project")
		(dolist (file (project-files project (list dir)))
			(when (string= (file-name-extension file) ext)
				(gptel-add-file file)))))

(declare-function gptel-preset "gptel-transient" (preset &optional setter))
(defvar joes-ai-project)

(defcustom joes-ai-gptel-preset-alist
	'((emacs-lisp-mode . emacs)
		(prog-mode . programming))
	"Alist mapping major modes to gptel presets, used by `joes-ai-gptel'.
Entries are checked in order with `derived-mode-p', so list more specific
modes first."
	:type '(alist :key-type symbol :value-type symbol)
	:group 'joe)

(defun joes-ai-gptel ()
	"Create a gptel chat buffer from the current buffer with a preset.
The preset is taken from `joes-ai-gptel-preset-alist' when the current buffer
visits a file, `chat' otherwise.  The chat buffer's project is the current
buffer's project."
	(interactive)
	(let* ((preset (or (and (buffer-file-name)
							(cdr (seq-find (lambda (entry) (derived-mode-p (car entry)))
											joes-ai-gptel-preset-alist)))
						'chat))
		   (project (project-current nil))
		   (buffer (gptel (generate-new-buffer-name
						  (format "*%s%s*" (gptel-backend-name gptel-backend)
								  (if project (format ":%s" (project-name project)) ""))))))
		(with-current-buffer buffer
			(gptel-preset preset (lambda (sym val) (set (make-local-variable sym) val)))
			(setq joes-ai-project project))
		(pop-to-buffer buffer)
		buffer))

(defun joes-ai-enforce-utf-8 ()
	"Set UTF-8 as the file coding system in gptel buffers."
	(set-buffer-file-coding-system 'utf-8))

;; --- End of configuration ---

(when mcp-hub-servers
	(gptel-mcp-connect nil t t))

(joes-keybinding-ai)

(provide 'joes-ai)

;;; joes-ai.el ends here
