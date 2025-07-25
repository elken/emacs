;;; lkn-lisp.el -- (((()))) -*- lexical-binding: t -*-
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
;;; We write Lisp languages often enough that they deserve their own
;;; module
;;; Code:

;;; Emacs Lisp
(defun lkn-elisp-setup ()
  "Setup function to be called before all Emacs Lisp buffers."
  (setq-local lkn/elisp-imenu-expressions
              `(("Section" "^[ \t]*;;;*\\**[ \t]+\\([^\n]+\\)" 1)
                ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
                ("Packages" "^\\s-*\\(?:;;;###package\\|(use-package?\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                ("Features" "^\\s-*\\(?:;;;###package\\|(use-feature?\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
                ("Modeline Segment" "(lkn-modeline-defsegment[[:space:]\n]+\\([^[:space:]\n)]+\\)" 1)
                ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
                ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
                ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
                ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
                ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\) +\\([^ ,)\n]+\\)" 1)
                ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)))
  (dolist (expression lkn/elisp-imenu-expressions)
    (add-to-list 'imenu-generic-expression expression)))

(add-hook 'emacs-lisp-mode-hook #'lkn-elisp-setup)

(defun lkn-elisp--calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.

Intended as :override advice for `calculate-lisp-indent'.

Adapted from URL `https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/'.  Optionally take PARSE-START."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with
  ;; `defvar` with it's value ommited, marking it special and only defining it
  ;; locally. So if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start)
             (beginning-of-defun))
            ((setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren. Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.  Indent under
                 ;; that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line except the
                      ;; first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      (or
                       ;; Align keywords in plists if each newline begins with
                       ;; a keyword. This is useful for "unquoted plist
                       ;; function" macros, like `map!' and `defhydra'.
                       (when-let* ((first (elt state 1))
                                   (char (char-after (1+ first))))
                         (and (eq char ?:)
                              (ignore-errors
                                (or (save-excursion
                                      (goto-char first)
                                      ;; FIXME Can we avoid `syntax-ppss'?
                                      (when-let* ((parse-sexp-ignore-comments t)
                                                  (end (scan-lists (point) 1 0))
                                                  (depth (ppss-depth (syntax-ppss))))
                                        (and (re-search-forward "^\\s-*:" end t)
                                             (= (ppss-depth (syntax-ppss))
                                                (1+ depth)))))
                                    (save-excursion
                                      (cl-loop for pos in (reverse (elt state 9))
                                               unless (memq (char-after (1+ pos)) '(?: ?\())
                                               do (goto-char (1+ pos))
                                               for fn = (read (current-buffer))
                                               if (symbolp fn)
                                               return (function-get fn 'indent-plists-as-data)))))))

                       ;; Check for quotes or backquotes around.
                       (let ((positions (elt state 9))
                             (quotep 0))
                         (while positions
                           (let ((point (pop positions)))
                             (or (when-let* ((char (char-before point)))
                                   (cond
                                    ((eq char ?\())
                                    ((memq char '(?\' ?\`))
                                     (or (save-excursion
                                           (goto-char (1+ point))
                                           (skip-chars-forward "( ")
                                           (when-let* ((fn (ignore-errors (read (current-buffer)))))
                                             (if (and (symbolp fn)
                                                      (fboundp fn)
                                                      ;; Only special forms and
                                                      ;; macros have special
                                                      ;; indent needs.
                                                      (not (functionp fn)))
                                                 (setq quotep 0))))
                                         (cl-incf quotep)))
                                    ((memq char '(?, ?@))
                                     (setq quotep 0))))
                                 ;; If the spelled out `quote' or `backquote'
                                 ;; are used, let's assume
                                 (save-excursion
                                   (goto-char (1+ point))
                                   (and (looking-at-p "\\(\\(?:back\\)?quote\\)[\t\n\f\s]+(")
                                        (cl-incf quotep 2)))
                                 (setq quotep (max 0 (1- quotep))))))
                         (> quotep 0))))
                     ;; Containing sexp has nothing before this line except the
                     ;; first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's almost
                 ;; certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset or
      ;; if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment or it does not apply
                ;; to this argument, try to align a constant-symbol under the
                ;; last preceding constant symbol, if there is such one of the
                ;; last 2 preceding symbols, in the previous uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation where it
                     ;; begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace following an
                       ;; open paren. (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant as
                ;; defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (normal-indent))))))

(advice-add #'calculate-lisp-indent :override #'lkn-elisp--calculate-lisp-indent)

(defvar lkn-elisp--face nil)
;;;###autoload
(defun lkn-elisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions.  Go until END."
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq lkn-elisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq lkn-elisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

(font-lock-add-keywords
 'emacs-lisp-mode
 (append `((lkn-elisp-highlight-vars-and-faces . lkn-elisp--face))))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package elisp-def
  :diminish elisp-def-mode
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package macrostep)

(use-package eros
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-i" . eros-inspect-last-result))
  :hook (emacs-lisp-mode . eros-mode))

(use-package package-lint-flymake
  :defer t
  :after flymake
  :config
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

(use-package clojure-mode
  :mode "\\.clj\\'")

(use-package cider
  :after clojure-mode)

(use-package racket-mode
  :mode "\\.rkt\\'")

(provide 'lkn-lisp)
;;; lkn-lisp.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
