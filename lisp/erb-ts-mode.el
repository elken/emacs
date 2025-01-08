;;; erb-ts-mode.el -- A tree-sitter mode for erb/ejs -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2025 Ellis Kenyő

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Update this when closer to an mvp
;;; TODO: Completion?
;;; TODO: Proper register as a mode
;;; Code:

(require 'treesit)
(require 'ruby-ts-mode)
(require 'html-ts-mode)
(require 'sgml-mode)

(defvar erb-ts-mode-font-lock-rules
  '(
    ;; ERB directives and their content
    :language embedded-template
    :feature directive
    ((directive
      "<%"  @font-lock-keyword-face
      (code) @font-lock-variable-name-face
      "%>" @font-lock-keyword-face)
     (output_directive
      "<%=" @font-lock-keyword-face
      (code) @font-lock-variable-name-face
      "%>" @font-lock-keyword-face)
     (comment_directive
      "<%#" @font-lock-comment-face
      "%>" @font-lock-comment-face))

    ;; Ruby code in embedded blocks
    ;; :language ruby
    ;; :feature keyword
    ;; ((constant) @font-lock-constant-face
    ;;  (method_call
    ;;   method: (identifier) @font-lock-function-name-face)
    ;;  (call
    ;;   method: (identifier) @font-lock-function-name-face)
    ;;  (identifier) @font-lock-variable-name-face)

    :language ruby
    :feature conditional
    :override t
    (["if" "end"] @font-lock-keyword-face
     (call method: (identifier) @font-lock-function-name-face))

    ;; Keep existing HTML rules
    :language html
    :feature tag
    ((element
      (start_tag (tag_name) @font-lock-function-name-face))
     (element
      (end_tag (tag_name) @font-lock-function-name-face))
     (self_closing_tag
      (tag_name) @font-lock-function-name-face))

    :language html
    :feature attribute
    ((attribute
      (attribute_name) @font-lock-variable-name-face)
     (quoted_attribute_value) @font-lock-string-face)

    :language html
    :feature comment
    ((comment) @font-lock-comment-face)))

(defvar erb-ts-mode--range-settings
  (treesit-range-rules
   :embed 'ruby
   :host 'embedded-template
   '((code) @capture)

   :embed 'html
   :host 'embedded-template
   '((content) @capture)

   ;; :embed 'javascript
   ;; :host 'html
   ;; :offset '(1 . -1)
   ;; '((script_element
   ;;    (start_tag)
   ;;    (raw_text) @capture))

   ;; :embed 'css
   ;; :host 'html
   ;; :offset '(1 . -1)
   ;; '((style_element
   ;;    (start_tag)
   ;;    (raw_text) @capture))
   ))

(defun erb-ts-mode--defun-name (node)
  "Identify the nearest defun for NODE."
  (when (equal (treesit-node-type node) "tag_name")
    (treesit-node-text node t)))

(defun erb-ts-mode--treesit-language-at-point (&optional point)
  "Return the language at POINT with improved error handling."
  (let* ((point (or point (point)))
         (parser (treesit-parser-create 'embedded-template))
         (node (and parser (treesit-node-at point parser)))
         (parent-node (and node (treesit-node-parent node)))
         (node-type (and node (treesit-node-type node)))
         (parent-type (and parent-node (treesit-node-type parent-node))))
    (cond
     ;; Ruby code contexts
     ((member node-type '("code"))
      'ruby)
     ;; Special elements
     ((and (equal node-type "raw_text")
           (equal parent-type "script_element"))
      'javascript)
     ((and (equal node-type "raw_text")
           (equal parent-type "style_element"))
      'css)
     ;; Template-specific nodes
     ((member node-type '("comment_directive"))
      'embedded-template)
     ;; Default case
     (t 'embedded-template))))

(defun erb-ts-setup ()
  "Setup treesit for `erb-ts-mode'."
  (setq-local electric-pair-pairs
              '((?\< . ?\>)
                (?\% . ?\%)
                (?\{ . ?\})
                (?\( . ?\))
                (?\[ . ?\])
                (?\' . ?\')
                (?\" . ?\")))

  ;; Comments and text content
  (setq-local treesit-text-type-regexp
              (regexp-opt '("comment" "text")))

  (setq-local treesit-font-lock-settings
              (append (ruby-ts--font-lock-settings 'ruby)
                      html-ts-mode--font-lock-settings
                      (apply #'treesit-font-lock-rules erb-ts-mode-font-lock-rules)))

  ;; Enhanced feature list
  (setq-local treesit-font-lock-feature-list
              '((comment builtin-variable builtin-constant keyword)
                (constant literal string interpolation type global instance)
                (method-definition parameter-definition builtin-function function assignment symbol)
                (error escape-sequence bracket punctuation definition property erb-delimiter tag attribute)))

  (setq-local treesit-language-at-point-function #'erb-ts-mode--treesit-language-at-point)
  (setq-local treesit-range-settings erb-ts-mode--range-settings)

  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode erb-ts-mode html-mode "ERB[ts]"
  "\nMajor mode for editing ERB with tree-sitter."
  :syntax-table html-mode-syntax-table

  (unless (treesit-ready-p 'ruby)
    (error "Tree-sitter grammar for Ruby isn't available"))

  (unless (treesit-ready-p 'css)
    (error "Tree-sitter grammar for Ruby isn't available"))

  (unless (treesit-ready-p 'javascript)
    (error "Tree-sitter grammar for Ruby isn't available"))

  (unless (treesit-ready-p 'html)
    (error "Tree-sitter grammar for HTML isn't available"))

  (unless (treesit-ready-p 'embedded-template)
    (error "Tree-sitter grammar for ERB isn't available"))

  (when (treesit-ready-p 'embedded-template)
    (setq-local treesit-primary-parser (treesit-parser-create 'embedded-template))
    (treesit-parser-create 'embedded-template)
    (erb-ts-setup)))

(defun erb-ts-debug-at-point ()
  "Debug info for node at point."
  (interactive)
  (let* ((point (point))
         (node (treesit-node-at point (treesit-parser-create 'embedded-template)))
         (parent (treesit-node-parent node)))
    (message "Current node: %s\nParent node: %s\nText: %s"
             (treesit-node-type node)
             (and parent (treesit-node-type parent))
             (treesit-node-text node t))))

(defun erb-ts-debug-ruby-tree ()
  "Debug Ruby parse tree at point."
  (interactive)
  (let* ((point (point))
         (node (treesit-node-at point (treesit-parser-create 'ruby)))
         (root (treesit-node-parent node)))
    (while (treesit-node-parent root)
      (setq root (treesit-node-parent root)))
    (message "Ruby tree:\n%s"
             (treesit-node-string root))))

(defun erb-ts-debug-node-ancestry (point)
  "Print detailed information about node ancestry at POINT."
  (interactive "d")
  (let* ((erb-parser (treesit-parser-create 'embedded-template))
         (ruby-parser (treesit-parser-create 'ruby))
         (erb-node (treesit-node-at point erb-parser))
         (ruby-node (treesit-node-at point ruby-parser)))

    ;; Print ERB node ancestry
    (let ((node erb-node))
      (message "ERB Node ancestry:")
      (while node
        (message "  %s: %s"
                (treesit-node-type node)
                (treesit-node-text node t))
        (setq node (treesit-node-parent node))))

    ;; Print Ruby node ancestry if exists
    (when ruby-node
      (let ((node ruby-node))
        (message "\nRuby Node ancestry:")
        (while node
          (message "  %s: %s"
                  (treesit-node-type node)
                  (treesit-node-text node t))
          (setq node (treesit-node-parent node)))))))

(defun erb-ts-debug-compare-if-statements ()
  "Compare the parse trees of different if statements in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "if" nil t)
        (message "\nIf statement #%d at position %d:" (cl-incf count) (point))
        (erb-ts-debug-node-ancestry (point))))))

(defun erb-ts-debug-code-nodes ()
  "Show all code nodes in the current buffer with their line info."
  (interactive)
  (let* ((parser (treesit-parser-create 'embedded-template))
         (query (treesit-query-compile 'embedded-template '((code) @code)))
         (captures (treesit-query-capture parser query)))
    (with-current-buffer (get-buffer-create "*erb-debug*")
      (erase-buffer)
      (dolist (capture captures)
        (when (eq (car capture) 'code)
          (let ((node (cdr capture)))
            (insert (format "=== Code Node ===\n")
                   "Content:\n"
                   (treesit-node-text node t)
                   "\n\n"))))
      (display-buffer (current-buffer)))))

(provide 'erb-ts-mode)
;;; erb-ts-mode.el ends here
