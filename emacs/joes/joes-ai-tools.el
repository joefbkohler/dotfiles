;;; joes-ai-tools.el --- AI assistant tools	 -*- lexical-binding: t; -*-

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
;;; TODO: Create/add tools to modify files and other Emacs functionality.
;;; Code:

(require 'gptel)
(require 'joes-utils)

(gptel-make-tool
	:name "search_web"
	:description "Search web for the given string. Returns a list of urls with a short description."
	:category "web"
	:async t
	:function (lambda (callback search-string)
				  (url-retrieve (concat "https://duckduckgo.com/html/?q=" search-string)
					  (lambda (_)
						  (funcall callback (joes-get-list-of-ddg-results (buffer-string)))
						  (kill-buffer (current-buffer)))
					  nil t t))
	:args (list '(:name "search-string"
					 :type string
					 :description "The string used to search the web.")))

(gptel-make-tool
	:name "read_url"
	:description "Returns the content of a web page."
	:category "web"
	:async t
	:function (lambda (ai-callback url)
				  (url-retrieve url
					  (lambda (_)
						  (re-search-forward "<head[^>]*>" nil t)
						  (let ((html-tree (libxml-parse-html-region (point) (point-max)))
								   (tag-to-remove '(style script nav img svg form button)))
							  
							  (dolist (tag tag-to-remove)
								  (dolist (node (dom-by-tag html-tree tag))
									  (dom-remove-node html-tree node)))
							  
							  (with-temp-buffer
								  (dom-print html-tree)
								  (let ((html-text (replace-regexp-in-string ">[[:space:]]+" " " (buffer-substring-no-properties (point-min) (point-max)))))
									  (funcall ai-callback
										  (if (length< html-text 16000) ;; 16000 characters max to avoid context overloading
											  html-text
											  (format "The content of the web page is too long: %i. Use local retriever." (length html-text)))))))
						  (kill-buffer (current-buffer)))
					  nil t t))
	:args (list '(:name "url"
					 :type string
					 :description "The url of the web page to open.")))

(provide 'joes-ai-tools)
;;; joes-ai-tools.el ends here.
