;;; erb-ts-mode.el -- A tree-sitter mode for erb/ejs -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2024 Ellis Kenyő

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
(require 'sgml-mode)

(defvar erb-ts-font-lock-rules
  '(:language embedded-template
    :feature output
    ((output_directive
      (code) @font-lock-variable-name-face))

    :language embedded-template
    :feature directive
    ((directive
      (code) @font-lock-preprocessor-face))
    
    :language embedded-template
    :feature comment
    ((comment_directive
      (comment) @font-lock-comment-face))
    
    :language embedded-template
    :feature erb-delimiter
    ((["<%=" "<%#" "<%" "%>" "-%>"] @font-lock-keyword-face))
    
    :language html
    :feature tag
    ;; Regular elements
    ((element
      (start_tag (tag_name) @font-lock-function-name-face))
     (element
      (end_tag (tag_name) @font-lock-function-name-face))
     (self_closing_tag
      (tag_name) @font-lock-function-name-face)
     ;; Special elements
     (script_element
      (start_tag (tag_name) @font-lock-keyword-face))
     (script_element
      (end_tag (tag_name) @font-lock-keyword-face))
     (style_element
      (start_tag (tag_name) @font-lock-keyword-face))
     (style_element
      (end_tag (tag_name) @font-lock-keyword-face)))

    :language html
    :feature attribute
    ((attribute
      (attribute_name) @font-lock-variable-name-face)
     (quoted_attribute_value) @font-lock-string-face)

    :language html
    :feature comment
    ((comment) @font-lock-comment-face)

    :language html
    :override t
    :feature declaration
    ((doctype) @font-lock-keyword-face)
    
    :language ruby
    :feature variable
    :override t
    ((call
      receiver: (identifier) @font-lock-variable-name-face) @_call
      (identifier) @font-lock-variable-name-face)

    :language ruby
    :feature method
    :override t  
    ((call
      method: (identifier) @font-lock-function-call-face) @_call))
  )  

(defun erb-ts-in-ruby-content-p (pos)
  "Check if POS is within a Ruby code block."
  (let* ((erb-parser (cl-find 'embedded-template (treesit-parser-list) :key #'treesit-parser-language))
         (node (treesit-node-at pos 'embedded-template)))
    (and node
         (member (treesit-node-type node) 
                '("code" "output_directive" "directive")))))

(defun erb-ts-ruby-block-range (pos)
  "Get the full range of the Ruby block at POS.
Returns cons cell (START . END) or nil if not in Ruby block."
  (let* ((erb-parser (cl-find 'embedded-template (treesit-parser-list) :key #'treesit-parser-language))
         (node (treesit-node-at pos 'embedded-template)))
    (when node
      (let ((parent (treesit-node-parent node)))
        (when (and parent (member (treesit-node-type parent)
                                '("output_directive" "directive")))
          (let ((code-node (treesit-node-child-by-field-name parent "code")))
            (when code-node
              (cons (treesit-node-start code-node)
                    (treesit-node-end code-node)))))))))

(defun erb-ts-update-ruby-ranges (ranges parser)
  "Update Ruby parser ranges based on ERB code blocks."
  (when-let ((ruby-parser (cl-find 'ruby (treesit-parser-list)
                                  :key #'treesit-parser-language)))
    (let* ((root (treesit-parser-root-node parser))
           (new-ranges '())
           (current-ranges (treesit-parser-included-ranges ruby-parser)))
      
      ;; Get ranges from both types of directives
      (dolist (node (treesit-query-capture 
                    root 
                    '((directive (code) @ruby)
                      (output_directive (code) @ruby))))
        (let* ((node (cdr node))
               (start (treesit-node-start node))
               (end (treesit-node-end node)))
          (push (cons start end) new-ranges)))
      
      ;; Sort ranges
      (setq new-ranges (sort new-ranges (lambda (a b) (< (car a) (car b)))))
      
      ;; Only update if ranges have actually changed
      (when (and new-ranges
                 (not (equal new-ranges current-ranges)))
        (treesit-parser-set-included-ranges ruby-parser new-ranges)))))

(defun erb-ts-handle-revert ()
  "Reset and update parser state after buffer revert."
  ;; Give the buffer a chance to stabilize
  (run-with-timer 0.1 nil
                  (lambda (buf)
                    (when (buffer-live-p buf)
                      (with-current-buffer buf
                        (when-let* ((erb-parser (cl-find 'embedded-template 
                                                       (treesit-parser-list) 
                                                       :key #'treesit-parser-language)))
                          ;; Force a fresh parse
                          (treesit-parser-set-included-ranges 
                           erb-parser 
                           (list (cons (point-min) (point-max))))
                          ;; Then update Ruby ranges
                          (erb-ts-update-ruby-ranges nil erb-parser)))))
                  (current-buffer)))

(defun erb-ts-setup ()
  "Setup treesit for erb-ts-mode."
  (setq-local electric-pair-pairs
              '((?\< . ?\>)
                (?\% . ?\%)
                (?\{ . ?\})
                (?\( . ?\))
                (?\[ . ?\])
                (?\' . ?\')
                (?\" . ?\")))
  
  (setq-local treesit-font-lock-settings
              (append (ruby-ts--font-lock-settings 'ruby)
                      (apply #'treesit-font-lock-rules
                             erb-ts-font-lock-rules)))
  
  (setq-local treesit-font-lock-feature-list
              '((erb-delimiter output directive comment)
                (tag attribute delimiter declaration)
                (keyword string const method-definition parameter-definition 
                         variable method builtin-variable builtin-constant 
                         builtin-function delimiter escape-sequence constant 
                         global instance interpolation literal symbol assignment)
                (bracket error function operator punctuation)))
  
  ;; Set up parsers and ranges
  (let ((ruby-parser (cl-find 'ruby (treesit-parser-list) :key #'treesit-parser-language))
        (html-parser (cl-find 'html (treesit-parser-list) :key #'treesit-parser-language))
        (erb-parser (cl-find 'embedded-template (treesit-parser-list) :key #'treesit-parser-language)))
    
    ;; Set up HTML parser to handle everything
    (treesit-parser-set-included-ranges html-parser 
                                       (list (cons (point-min) (point-max))))
    
    ;; Initial range setup for Ruby
    (erb-ts-update-ruby-ranges nil erb-parser)
    
    ;; Add after-change hook to update ranges
    (add-hook 'after-change-functions
              (lambda (beg end len)
                (erb-ts-update-ruby-ranges 
                 (list (cons beg end))
                 erb-parser))
              nil t))
  
  (treesit-major-mode-setup))


(defvar-local erb-ts-debug-overlays nil
  "List of overlays used for debugging Ruby regions in ERB.")

(defface erb-ts-debug-region-face
  '((t :background "#224422" :extend t))
  "Face used to highlight Ruby regions in ERB debug mode.")

(defun erb-ts-debug-clear-overlays ()
  "Clear all debug overlays."
  (mapc #'delete-overlay erb-ts-debug-overlays)
  (setq erb-ts-debug-overlays nil))

(defun erb-ts-debug-highlight-ranges ()
  "Highlight current Ruby parser ranges with overlays."
  (erb-ts-debug-clear-overlays)
  (when-let* ((ruby-parser (cl-find 'ruby (treesit-parser-list) :key #'treesit-parser-language))
              (ranges (treesit-parser-included-ranges ruby-parser)))
    (dolist (range ranges)
      (let* ((start (car range))
             (end (cdr range))
             (ov (make-overlay start end)))
        (overlay-put ov 'face 'erb-ts-debug-region-face)
        (overlay-put ov 'help-echo (format "Ruby region: %S" range))
        (push ov erb-ts-debug-overlays)))))

(defun erb-ts-debug-print-ranges ()
  "Print information about current Ruby parser ranges."
  (interactive)
  (when-let* ((ruby-parser (cl-find 'ruby (treesit-parser-list) :key #'treesit-parser-language))
              (ranges (treesit-parser-included-ranges ruby-parser)))
    (with-current-buffer (get-buffer-create "*erb-debug*")
      (erase-buffer)
      (insert "=== Current Ruby Parser Ranges ===\n\n")
      (dolist (range ranges)
        (insert (format "%d-%d: %S\n" 
                       (car range) (cdr range)
                       (buffer-substring-no-properties (car range) (cdr range)))))
      (display-buffer (current-buffer)))))

(define-minor-mode erb-ts-debug-mode
  "Toggle ERB tree-sitter debug mode.
Shows overlays for Ruby parser ranges and updates them as you type."
  :lighter " ERBDbg"
  :global nil
  (if erb-ts-debug-mode
      (progn
        (erb-ts-debug-highlight-ranges)
        (add-hook 'after-change-functions 
                  (lambda (_beg _end _len)
                    (run-with-timer 0.05 nil #'erb-ts-debug-highlight-ranges))
                  nil t))
    (progn
      (remove-hook 'after-change-functions #'erb-ts-debug-highlight-ranges t)
      (erb-ts-debug-clear-overlays))))

;;;###autoload
(define-derived-mode erb-ts-mode sgml-mode "ERB[ts]"
  "\nMajor mode for editing ERB with tree-sitter."
  :syntax-table sgml-mode-syntax-table

  (setq-local font-lock-defaults nil)
  (add-hook 'after-revert-hook #'erb-ts-handle-revert nil t)

  (when (treesit-ready-p 'embedded-template)
    (treesit-parser-create 'embedded-template)
    (treesit-parser-create 'html)
    (treesit-parser-create 'ruby)
    (erb-ts-setup)))

(provide 'erb-ts-mode)
;;; erb-ts-mode.el ends here
