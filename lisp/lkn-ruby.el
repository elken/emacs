;;; lkn-ruby.el -- The least shit Kaiser Chiefs song -*- lexical-binding: t -*-
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
;;; Currently my $WORKLANG, this will end up having a fair amount of
;;; config. A lot of the power will come from LSP, and I lean quite a
;;; bit on that.
;;; Code:

(use-package rbenv
  :defer t
  :hook ((ruby-ts-mode ruby-mode) . global-rbenv-mode))

(use-package rspec-mode
  :custom
  (rspec-use-spring-when-possible nil)
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (ad-activate 'rspec-compile))

(use-package erb-ts-mode
  :ensure nil
  :mode "\\.erb$"
  :config
  (defvar erb-translation-capf--properties
    (list :annotation-function (lambda (snippet)
				 (message "%S" snippet)
				 snippet)
          :company-kind (lambda (_) 'string)
          :exclusive 'no)
    "Completion extra properties for `erb-translation-capf'.")
  
  (defun erb-translate-capf--in-t-call-args-p ()
    "Return cons of start/end positions if point is within t() args, nil otherwise."
    (let* ((parser (cl-find 'ruby (treesit-parser-list) :key #'treesit-parser-language))
           (matches (treesit-query-capture 
                     parser
                     '((program (call method: ((identifier) @m)
				      arguments: _ @args
				      (:match "^t$" @m)))
                       (call method: ((identifier) @m)
                             arguments: _ @args
                             (:match "^t$" @m)))
                     (point-min) (point-max))))
      (seq-some (lambda (match)
		  (when (eq 'args (car match))
                    (let* ((args-node (cdr match))
                           ;; Adjust bounds to skip the parentheses
                           (start (+ 2 (treesit-node-start args-node)))
                           (end (- (treesit-node-end args-node) 2)))
                      (when (and (>= (point) start)
				 (<= (point) end))
			(cons start end)))))
		matches)))

  (defun erb-translation-capf--filter (str)
    "Use STR to compute and filter a new completion table."
    (cons ))
  
  (defun erb-translation-capf--completion-table ()
    "Return a suitable function to create a completion table."
    (lambda (str pred action)
      (let ((results (erb-translation-capf--filter str)))
	(if (eq action 'metadata)
	    '(metadata (category . erb-translation-capf))
	  (complete-with-action action results str pred)))))
  
  (defun erb-translation-capf (&optional interactive)
    (interactive (list t))
    (if interactive
	(let ((completion-at-point-functions #'erb-translation-capf))
          (message "Current buffer content at point: %s" 
                   (when-let ((bounds (erb-translate-capf--in-t-call-args-p)))
                     (buffer-substring-no-properties (car bounds) (cdr bounds))))
          (or (completion-at-point) 
              (user-error "erb-translation-capf: No completions")))
      (when-let ((bounds (erb-translate-capf--in-t-call-args-p)))
	`(,(car bounds) ,(cdr bounds)
          ,(completion-table-with-cache
            (lambda (string)
              (all-completions 
               (string-trim string "'" "'")
               '(".page_title" ".heading"))))
          ,@erb-translation-capf--properties)))))

(provide 'lkn-ruby)
;;; lkn-ruby.el ends here
