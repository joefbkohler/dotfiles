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
(require 'diff-mode)
(require 'xdg)

(defcustom joes-ai-read-url-max-chars 16000
	"Maximum characters returned by the read_url tool.
Avoids context overloading.	 nil means no limit."
	:group 'joe
	:type '(choice (const :tag "No limit" nil) integer))

;; -- Helper functions

(defun joes-ai-show-patches ()
	"Show all saved AI patches in the `*AI Buffer*' diff buffer, read-only."
	(interactive)
	(let ((dir (joes-ai-patches-cache-dir))
			 (buf (get-buffer-create "*AI Patches*")))
		(pop-to-buffer buf)
		(setq inhibit-read-only t)
		(erase-buffer)
		(when (file-directory-p dir)
			(dolist (file (directory-files dir t "\\.patch\\'"))
				(insert-file-contents file)
				(goto-char (point-max))
				(unless (bolp) (insert "\n"))))
		(diff-mode)
		(read-only-mode 1)
		(goto-char (point-min))))

(defun joes-ai-accept-patches ()
	"Accept the saved AI patches by deleting them and the `*AI Patches*'."
	(interactive)
	(when (y-or-n-p "Accept all AI patches and delete the saved copies? ")
		(let ((dir (joes-ai-patches-cache-dir)))
			(when (file-directory-p dir)
				(delete-directory dir t)))
		(when-let* ((buf (get-buffer "*AI Buffer*")))
			(kill-buffer buf))
		(message "AI patches accepted.")))

(defun joes-ai-patches-cache-dir ()
	"Return the directory where applied patches are saved."
	(expand-file-name "joes-ai/patches/" (xdg-cache-home)))


;; ----------------

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
										  (if (or (null joes-ai-read-url-max-chars)
												  (length< html-text joes-ai-read-url-max-chars))
											  html-text
											  (format "The content of the web page is too long: %i. Use local retriever." (length html-text)))))))
						  (kill-buffer (current-buffer)))
					  nil t t))
	:args (list '(:name "url"
					 :type string
					 :description "The url of the web page to open.")))

(gptel-make-tool
	:name "find_files"
	:description "Find files whose names match a regexp under a directory. Returns a list of absolute file paths."
	:category "filesystem"
	:function (lambda (directory regexp)
				  (let ((files (directory-files-recursively
								   (expand-file-name directory) regexp)))
					  (mapconcat #'identity files "\n")))
	:args (list '(:name "directory"
					 :type string
					 :description "Directory to search in.")
			  '(:name "regexp"
				   :type string
				   :description "Regexp matched against file names.")))

(gptel-make-tool
	:name "rgrep"
	:description "Search files under a directory for a regexp. Returns matching lines as file:line:text."
	:category "filesystem"
	:async t
	:function (lambda (callback directory regexp file-glob)
				  (let ((default-directory (expand-file-name directory)))
					  (save-window-excursion
						  (joes-compilation-buffer-string-async callback "*grep*")
						  (rgrep regexp file-glob default-directory))))
	:args (list '(:name "directory"
					 :type string
					 :description "Directory to search in.")
			  '(:name "regexp"
				   :type string
				   :description "Regexp to search for in file contents.")
			  '(:name "file-glob"
				   :type string
				   :description "Glob matched against file names to include, e.g. \"*.el\".")))

(gptel-make-tool
	:name "read_file"
	:description "Visit a file and return its contents. If start-line and end-line are given (1-based, inclusive), return only those lines."
	:category "filesystem"
	:function (lambda (file-path &optional start-line end-line)
				  (with-current-buffer (find-file-noselect (expand-file-name file-path))
					  (if (not (or start-line end-line))
						  (buffer-substring-no-properties (point-min) (point-max))
						  (let ((start-point (save-excursion
												 (goto-char (point-min))
												 (forward-line (1- (or start-line 1)))
												 (point)))
								   (end-point (if end-line
												  (save-excursion
													  (goto-char (point-min))
													  (forward-line (1- end-line))
													  (line-end-position))
												  (point-max))))
							  (buffer-substring-no-properties start-point end-point)))))
	:args (list '(:name "file-path"
					 :type string
					 :description "Path of the file to read.")
			  '(:name "start-line"
				   :type integer
				   :optional t
				   :description "First line to return, 1-based.")
			  '(:name "end-line"
				   :type integer
				   :optional t
				   :description "Last line to return, 1-based and inclusive.")))

(gptel-make-tool
	:name "write_files"
	:description "Write to files by applying a multi-file unified diff. Nothing is changed if any hunk does not match the current files."
	:category "filesystem"
	:function (lambda (patch)
				  (with-temp-buffer
					  (insert patch)
					  (diff-mode)
					  (if-let* ((failures (diff-apply-buffer)))
						  (error "%d hunk(s) failed; no files changed" failures)
						  (let ((dir (joes-ai-patches-cache-dir)))
							  (make-directory dir t)
							  (write-region patch nil
								  (expand-file-name
									  (format "%s-%06d.patch"
										  (format-time-string "%Y%m%d-%H%M%S") (random 1000000))
									  dir)
								  nil 'silent)
							  (joes-ai-show-patches)
							  "Patch applied and saved."))))
	:args (list '(:name "patch"
					 :type string
					 :description "Unified diff with absolute file paths.")))

(provide 'joes-ai-tools)
;;; joes-ai-tools.el ends here.
