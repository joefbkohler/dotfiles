;;; joes-ai-directives.el --- AI assistant directives	 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Joe Köhler

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

(require 'gptel)
(require 'project)
(require 'joes-ai-tools)

(declare-function magit-get-current-branch "magit-branch")

(defvar joes-ai-project nil
	"Project used by the AI directives.	 Set with `joes-ai-set-project'.")

(defun joes-ai-set-project ()
	"Set `joes-ai-project' to the current project.
Nil projects are ignored and keep the previous value."
	(interactive)
	(when-let* ((project (project-current)))
		(setq joes-ai-project project)))

(defun joes-ai-project-info ()
	"Return project root and git branch block, or an empty string outside a project."
	(if-let* ((project joes-ai-project)
				 (root (expand-file-name (project-root project))))
		(progn
			(require 'magit-branch)
			(format "\n[Project Context]\nRoot: %s\nBranch: %s"
				root
				(let ((default-directory root))
					(or (magit-get-current-branch) "detached"))))
		""))

(defun joes-ai-system-append (system)
	"Return a :system spec that appends SYSTEM to the inherited system message."
	(list :function
		(lambda (parent)
			(lambda ()
				(concat (if (functionp parent) (funcall parent) parent) system)))))

(defvar-local joes-ai--planning-saved nil
	"Session state saved by `joes-ai-toggle-planning', or nil when planning is off.")

(defconst joes-ai-planning-instructions "\n[Planning Mode]
You're in Planning mode! You can't write to files or modify the system. When a task is requested, create a step by step plan, outline the features, functions, algorithm, main function calls, or commands. Avoid directly generating code or commands unless directly requested.")

(defun joes-ai-toggle-planning ()
	"Toggle the planning state in the current buffer's gptel session.
When off, save the system message and tools, then append the planning
instructions and remove write tools.  When on, restore the saved state."
	(interactive)
	(if (and joes-ai--planning-saved
			(string-search joes-ai-planning-instructions
				(if (functionp gptel-system-prompt) (funcall gptel-system-prompt) gptel-system-prompt)))
		(progn
			(setq-local gptel-system-prompt (plist-get joes-ai--planning-saved :system))
			(setq-local gptel-tools (plist-get joes-ai--planning-saved :tools))
			(setq joes-ai--planning-saved nil)
			(message "Planning off"))
		(setq joes-ai--planning-saved
			(list :system gptel-system-prompt :tools gptel-tools))
		(gptel--apply-preset
			`(:system ,(joes-ai-system-append joes-ai-planning-instructions)
				:tools (:function ,(lambda (tools)
									(cl-remove-if
										(lambda (tool)
											(equal (gptel-tool-category tool) "filesystem-write"))
										tools))))
			(lambda (sym val) (set (make-local-variable sym) val)))
		(message "Planning on")))

(setopt gptel-directives
	`((default . "[General Instruction]
You are an AI Agent living inside Emacs. Answer like a command line tool would answer: with a single, dry, direct answer, without pleasantries, or summaries.
[User Profile]
Your user is an experienced software engineer. He can understand code, hardware details, and other technical concepts without explanation. He is used to writing and reading requirements and specifications. Limit your code and commands explanations to usage, instead of implementation details.
[Answers Instructions]
Follow this rules when answering questions:
* 'Not enough information' is a valid answer. Unless ordered not to, you should request further information when you don't have enough to answer.
* Never assume temporal information, like date, weather, or other real world real time information.
* Provide only a single answer, unless requested otherwise.
* When there are more than one way of doing something, provide ONLY ONE way, and briefly list the other options at the end. The user will request for more information if needed.
* Provide factual answers only. Do not provide opinions, or advices, unless requested.
[Emoji Usage Instruction]
Avoid emoji. Do not use emoji in a paragraph, or in a title. The only place emojis are acceptable are as values of a table, like the check mark ✅, the negative ❌, and others.
You can use the Warning emoji ⚠️ in a title to inform of dangers or caveats.
** Never use emojis in code, or any other generated content intended to be copied or directly used **
[Tool Use]
Use available tools to confirm temporal information that were not provided. You can use read-only tools liberaly, but only use write/modify tools when the user's request explicitly requires it. Never use write tools to answer questions, or when the user request is to just generate something. On tool error, you may try again with different parameters, up to 4 times, but always warn the user of the error.")
		 (rewrite . "[Rewriting Instructions]
* Add no markdown or markup code fences.
* Output exclusivelly the new text, without any explanations or comments.
* Follow the instructions to rewrite the provided snippet.")))

(defun joes-ai-os-info ()
	"Return operating system block."
	(format "\n[Operating System]\nName: %s\nType: %s\nConfiguration: %s"
		(pcase system-type
			('darwin "macOS")
			('gnu/linux "Linux")
			('windows-nt "Windows")
			('cygwin "Windows (Cygwin)")
			('android "Android")
			('berkeley-unix "BSD")
			(_ (symbol-name system-type)))
		system-type
		system-configuration))

(defun joes-ai-default-system-message ()
	"Return the default system directive string."
	(concat (alist-get 'default gptel-directives) (joes-ai-project-info) (joes-ai-os-info)))

(gptel-make-preset 'disable-thinking
	:temperature 1.0
	:system #'joes-ai-default-system-message
	:tools nil
	:request-params '(:eval joes-ai-chat-reasoning-off))

(gptel-make-preset 'chat
	:tools '("web" "filesystem-read" "filesystem-write")
	:system #'joes-ai-default-system-message
	:request-params '(:eval joes-ai-chat-reasoning-on)
	:temperature 1.0)

(gptel-make-preset 'programming
	:parents 'chat
	:tools '(:append ("programming"))
	:temperature 0.5
	:system (joes-ai-system-append "\n[Programming Instructions]
* When answering a question about code or other technical content, explain usage, and warnings, but not implementation details.
* When creating new features, or functions, look or request for references in the working project and follow its pattern.
* Check for errors after editing a file. If issues are found, tell the User, offer possible solutions and request input."))

(gptel-make-preset 'emacs-programming
	:parents 'programming
	:tools '(:function (lambda (tools)
						   (cl-remove-if
							   (lambda (tool)
								   (equal (gptel-tool-category tool) "programming"))
							   tools))
				:append ("emacs" "flymake_diagnostics"))
	:temperature 0.5
	:system (joes-ai-system-append
				(format "\n[Emacs Programming Instructions]
You're inside a running emacs instance.
Config root is %s.
Lisp directory is %s
User packages directory is %s
System packages directory are %s
* Target GNU Emacs Lisp unless stated otherwise.
* Use tabs for identation.
* Prefer built-in Emacs facilities over third-party packages.
* If a third-party package solves a problem, suggest it and ask the User's input.
* Prefer elisp functions call over shell commands, even if internally the lisp function uses shell." (file-name-directory user-init-file) lisp-directory package-user-dir package-directory-list)))

(defvar joes-ai-commit-message-model nil)
(gptel-make-preset 'commit-message
	:parents 'disable-thinking
	:use-context 'system
	:include-reasoning nil
	:model (or joes-ai-commit-message-model (default-value 'gptel-model))
	:system (joes-ai-system-append "\n[Commit Message Instructions]
* Be concise and direct. Focus on the WHAT, not the WHY.
* Use precise language, avoid subjective words like better, easier, faster, enhances, improves, etc...
* Instead of descriptions like 'improved function', just describe what was done.
* When describing a change, only describe the change, not the expected effect.
* Pay attention to whitespace and style changes.
* Answer with no markup guards nor explanation.
* Use `Conventional Commit' format:
<type>: <description>

[optional body]
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

(provide 'joes-ai-directives)
;;; joes-ai-directives.el ends here
