;;; joes-ai.el --- AI assistant configuration        -*- lexical-binding: t; -*-

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

;; This configuration is incomplete.  Add the following code to your
;; local Emacs file.  In my case: ~/.emacs.d/local.el
;; -- (plist-put minuet-openai-fim-compatible-options :end-point "http://joe-pc:11435/v1/completions")
;; -- (plist-put minuet-openai-fim-compatible-options :model "qwen2.5-coder:3b")
;; -- (setq gptel-backend (gptel-make-ollama "Ollama Local"
;; 					  :stream t
;; 					  :models '(deepseek-r1 qwen2.5-coder:3b qwen2.5-coder:7b)
;; 					  :endpoint "/api/chat"))
;;; Code:

(require 'minuet)

(setq minuet-provider 'openai-fim-compatible)
(setq minuet-n-completions 2)
(setq minuet-context-window 512)
(setq minuet-request-timeout 3)
(plist-put minuet-openai-fim-compatible-options :api-key "TERM")
(plist-put minuet-openai-fim-compatible-options :name "Ollama")
(minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 50)
(minuet-set-optional-options minuet-openai-fim-compatible-options :temperature 0.2)

(provide 'joes-ai)
;;; joes-ai.el ends here
