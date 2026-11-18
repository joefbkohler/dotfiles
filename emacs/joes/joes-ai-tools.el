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
;;; Code:

(require 'gptel)
(require 'joes-utils)
(require 'xdg)
(require 'flymake)
(require 'joes-ai-edit-tool)
(require 'package)

(defcustom joes-ai-read-url-max-chars 16000
	"Maximum characters returned by the read_url tool.
Avoids context overloading.	 nil means no limit."
	:group 'joe
	:type '(choice (const :tag "No limit" nil) integer))

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
	:category "filesystem-read"
	:function (lambda (directory regexp)
				  (let ((files (directory-files-recursively
								   (expand-file-name directory) regexp nil t)))
					  (mapconcat #'identity files "\n")))
	:args (list '(:name "directory"
					 :type string
					 :description "Directory to search in.")
			  '(:name "regexp"
				   :type string
				   :description "Regexp matched against file names.")))

(gptel-make-tool
	:name "rgrep"
	:description "Search files under a directory for a regexp. Return file path, line, and text of matches."
	:category "filesystem-read"
	:async t
	:function (lambda (callback directory regexp file-glob)
				  (let* ((default-directory (expand-file-name directory))
						 (buffer-name (generate-new-buffer-name "*rgrep-ai*"))
						 (compilation-buffer-name-function (lambda (_mode) buffer-name)))
					  (save-window-excursion
						  (joes-compilation-buffer-async
							  (lambda (buf)
								  (funcall callback (joes-rgrep-extract-matches buf))
								  (kill-buffer buf))
							  buffer-name)
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
	:category "filesystem-read"
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
	:name "emacs_function_definition"
	:description "Return the whole body of an elisp function"
	:category "emacs"
	:function (lambda (function-name)
				  (let ((function-symbol (intern function-name)))
					  (if (fboundp function-symbol)
						  (let* ((buffer-and-point (find-function-noselect function-symbol))
									(buffer (car buffer-and-point))
									(point (cdr buffer-and-point)))
							  (with-current-buffer buffer
								  (goto-char point)
								  (let ((start (line-number-at-pos)))
									  (end-of-defun)
									  (buffer-substring-no-properties start (point)))))
						  "Function doesn't exist or not loaded")))
	:args (list '(:name "function-name"
					 :type string
					 :description "Name of the function")))

(gptel-make-tool
	:name "emacs_describe_symbol"
	:description "Return the documentation of an Emacs Lisp function or variable."
	:category "emacs"
	:function (lambda (name)
				  (let ((symbol (intern-soft name)))
					  (or (cond
						  ((null symbol) (format "No symbol named %s" name))
						  ((fboundp symbol) (documentation symbol t))
						  ((boundp symbol)
							  (documentation-property symbol 'variable-documentation t)))
						  (format "No documentation for %s" name))))
	:args (list '(:name "symbol"
					 :type string
					 :description "Name of the function or variable to describe.")))

(gptel-make-tool
	:name "list_installed_packages"
	:description "Return the list of packages installed via package.el, one name-version per line."
	:category "emacs"
	:function (lambda ()
				  (if (null package-alist)
					  "No installed packages."
				  (mapconcat
					  (lambda (entry)
						  (mapconcat #'package-desc-full-name (cdr entry) "\n"))
					  package-alist
					  "\n")))
	:args nil)

(gptel-make-tool
	:name "flymake_diagnostics"
	:description "Return the flymake diagnostics (errors, warnings, notes) for a file."
	:category "programming"
	:confirm nil
	:async t
	:function (lambda (callback file)
				  (if (not (file-name-absolute-p file))
					  (funcall callback (format "Error: path must be absolute: %s" file))
					  (let* ((path (expand-file-name file))
								(buf (or (find-buffer-visiting path)
										 (find-file-noselect path)))
								(deadline (+ (float-time) 30)))
						  (cl-labels
							  ((format-diags ()
								   (with-current-buffer buf
									   (let ((diags (flymake-diagnostics)))
										   (if (null diags)
											   "No flymake diagnostics."
											   (mapconcat
												   (lambda (d)
													   (let* ((beg (flymake-diagnostic-beg d))
																 (line (line-number-at-pos beg t))
																 (col (save-excursion
																		  (goto-char beg)
																		  (current-column))))
														   (format "%s:%d:%d: %s: %s"
															   path line col
															   (string-remove-prefix ":" (format "%s" (flymake-diagnostic-type d)))
															   (flymake-diagnostic-text d))))
												   diags
												   "\n")))))
								  (poll ()
									  (cond
										  ((> (float-time) deadline)
											  (funcall callback
												  (format "Error: timed out waiting for flymake on %s" path)))
										  ((with-current-buffer buf
											   (cl-set-difference (flymake-running-backends)
												   (flymake-reporting-backends)))
											  (run-with-timer 0.2 nil #'poll))
										  (t
											  (funcall callback (format-diags))))))
							  (with-current-buffer buf
								  (unless flymake-mode
									  (flymake-mode 1))
								  (flymake-start))
							  (run-with-timer 0.2 nil #'poll)))))
	:args (list '(:name "file"
					 :type string
					 :description "Absolute path of the file to inspect.")))

(gptel-make-tool
	:name "find_references"
	:description "Find references to a symbol. Returns the number of references, and one file:line: text entry per reference."
	:category "programming"
	:confirm nil
	:function (lambda (file symbol-name line-text &optional line)
				  (condition-case err
					  (progn
						  (unless (file-name-absolute-p file)
							  (user-error "File must be an absolute path: %s" file))
						  (with-current-buffer (find-file-noselect file)
							  (save-excursion
								  (joes-goto-symbol symbol-name line-text line)
								  (joes-xrefs-text
								   (lambda () (xref-find-references symbol-name))))))
					(error (format "Error: %s" (error-message-string err)))))
	:args (list '(:name "file"
					 :type string
					 :description "Absolute path of the file containing the symbol.")
			  '(:name "symbol-name"
				   :type string
				   :description "Identifier to look up, e.g. foo-bar.")
			  '(:name "line-text"
				   :type string
				   :description "Full text of the line containing the symbol, without leading or trailing whitespace.")
			  '(:name "line"
				   :type integer
				   :optional t
				   :description "1-based line number. Set it when a previous call reported more than one matching line.")))

(gptel-make-tool
	:name "find_implementations"
	:description "Find implementations or derived classes of a symbol (interface, class, etc...). It returns indirect implementations/inheritance as well as direct implementation/inheritance. Returns the number of implementations, and one file:line: text entry per implementation."
	:category "programming"
	:confirm nil
	:function (lambda (file symbol-name line-text &optional line)
				  (declare-function eglot-current-server "eglot")
				  (declare-function eglot-find-implementation "eglot")
				  (condition-case err
					  (progn
						  (unless (featurep 'eglot)
							  (user-error "Eglot is not loaded"))
						  (unless (file-name-absolute-p file)
							  (user-error "File must be an absolute path: %s" file))
						  (with-current-buffer (find-file-noselect file)
							  (unless (eglot-current-server)
								  (user-error "No Eglot server manages %s" file))
							  (save-excursion
								  (joes-goto-symbol symbol-name line-text line)
								  (joes-xrefs-text #'eglot-find-implementation))))
					  (error (format "Error: %s" (error-message-string err)))))
	:args (list '(:name "file"
					 :type string
					 :description "Absolute path of the file containing the symbol.")
			  '(:name "symbol-name"
				   :type string
				   :description "Identifier to look up, e.g. foo-bar.")
			  '(:name "line-text"
				   :type string
				   :description "Full text of the line containing the symbol, without leading or trailing whitespace. Must be unique within the file.")
			  '(:name "line"
				   :type integer
				   :optional t
				   :description "1-based line number. Set it when a previous call reported more than one matching line.")))

(gptel-make-tool
	:name "find_definition"
	:description "Find the definition/declaration file and position of a symbol (function, class, variable, etc...). Returns file:line: text entry."
	:category "programming"
	:confirm nil
	:function (lambda (file symbol-name line-text &optional line)
				  (condition-case err
					  (progn
						  (unless (file-name-absolute-p file)
							  (user-error "File must be an absolute path: %s" file))
						  (with-current-buffer (find-file-noselect file)
							  (save-excursion
								  (joes-goto-symbol symbol-name line-text line)
								  (joes-xrefs-text
								   (lambda () (xref-find-definitions symbol-name))))))
					(error (format "Error: %s" (error-message-string err)))))
	:args (list '(:name "file"
					 :type string
					 :description "Absolute path of the file containing the symbol.")
			  '(:name "symbol-name"
				   :type string
				   :description "Identifier to look up, e.g. foo-bar.")
			  '(:name "line-text"
				   :type string
				   :description "Full text of the line containing the symbol, without leading or trailing whitespace. Must be unique within the file.")
			  '(:name "line"
				   :type integer
				   :optional t
				   :description "1-based line number. Set it when a previous call reported more than one matching line.")))

(provide 'joes-ai-tools)
;;; joes-ai-tools.el ends here.
