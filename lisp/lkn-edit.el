;;; lkn-edit.el -- Emacs is a great operating system lacking a good text editor -*- lexical-binding: t -*-
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
;;; General purpose editing bits n bobs and tree-sitter.
;;; Why doesn't this just merge with meow/evil/whatever?  The point of
;;; these is being able to separate blocks out logically as well as
;;; being able to easily disable them.
;;; Code:

(use-package avy
  :bind
  ("C-'" . avy-goto-char))

(use-package tempel
  :custom
  (tempel-path (expand-file-name "templates.eld" user-emacs-directory))
  :bind
  (("C-c s" . tempel-complete)
   (:map tempel-map
         ([tab] . tempel-next)
         ([backtab] . tempel-previous)))
  :config
  (defun lkn-snippets-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let* ((template (alist-get (cadr elt) (tempel--templates))))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'lkn-snippets-include))

(use-package treesit-auto
  :disabled t
  :when (treesit-available-p)
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                            :lang 'embedded-template
                                            :ts-mode 'erb-ts-mode
                                            :remap 'web-mode
                                            :url "https://github.com/tree-sitter/tree-sitter-embedded-template"
                                            :ext "\\.erb\\"))
  (add-to-list 'treesit-auto-recipe-list (make-treesit-auto-recipe
                                          :lang 'jsdoc
                                          :ts-mode 'js-ts-mode
                                          :url "https://github.com/tree-sitter/tree-sitter-jsdoc"))
  (global-treesit-auto-mode))

(use-package elixir-ts-mode
  :hook ((elixir-mode elixir-ts-mode) . eglot-ensure)
  :after treesit-auto)

(use-package smartparens
  :defer t
  :hook ((prog-mode text-mode org-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :hook (clojure-mode . paredit-mode)
  :hook (lisp-mode . paredit-mode)
  :hook (common-lisp-mode . paredit-mode)
  :hook (racket-mode . paredit-mode))

(use-package jinx
  :init (global-jinx-mode)
  :diminish (jinx-mode)
  :custom
  (jinx-languages "en_GB")
  (jinx-include-modes '(text-mode prog-mode))
  (jinx-include-faces
   '((prog-mode font-lock-doc-face)
     (conf-mode font-lock-comment-face)))
  (jinx-exclude-regexps
   '((t "[A-Z]+\\>"
      "\\<[[:upper:]][[:lower:]]+\\>"
      "\\w*?[0-9\.'\"-]\\w*"
      "[a-z]+://\\S-+"
      "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
  :bind
  (("M-$" . jinx-correct)))

(use-package apheleia
  :hook ((ruby-mode ruby-ts-mode) . apheleia-mode)
  :config
  (cl-defun save-buffer-maybe-format (orig-fn &optional arg)
    "Allow universal argument to disable formatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall orig-fn)))
  (advice-add 'save-buffer :around #'save-buffer-maybe-format)

  ;; Custom function to format INI/Config files
  (cl-defun lkn/ini-indent (&key buffer scratch callback &allow-other-keys)
    "Format an ini BUFFER.
Use SCRATCH as a temporary buffer and CALLBACK to apply the transformation.

For more detail, see `apheleia--run-formatter-function'."
    (interactive)
    (with-current-buffer scratch
      (funcall (with-current-buffer buffer major-mode))

      (when (fboundp 'editorconfig-apply)
        (editorconfig-apply))

      (goto-char (point-min))
      (let ((indent-amount (if (boundp 'tab-width) tab-width 4))
            (inhibit-message t)
            (message-log-max nil))
        (while (not (eobp))
          (beginning-of-line)
          (cond
           ((looking-at "[ \t]*\\[")
            (delete-horizontal-space))
           ((or (looking-at "[ \t]*$")
                (looking-at "[ \t]*[#;]"))
            nil)
           (t
            (delete-horizontal-space)
            (indent-to indent-amount)))
          (forward-line 1)))
      (funcall callback)))

  (setf (alist-get 'ini-cfg apheleia-formatters) #'lkn/ini-indent)
  (setf (alist-get 'conf-mode apheleia-mode-alist) '(ini-cfg))
  (setf (alist-get 'conf-unix-mode apheleia-mode-alist) '(ini-cfg))

  ;; Replace the built-in rubocop to use our config
  (setf (alist-get 'rubocop apheleia-formatters)
        '("apheleia-from-project-root"
          ".rubocop.yml"
          "bundle"
          "exec"
          "rubocop"
          "-a"
          "--stderr"
          "--stdin" filepath
          "--format" "quiet"
          "--fail-level" "fatal"))

  ;; Add Ruby file types to use rubocop formatter
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(rubocop))
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
        '(rubocop))
  (setf (alist-get 'enh-ruby-mode apheleia-mode-alist)
        '(rubocop)))

(use-package undo-fu
  :bind (("C-z" . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo))
  :custom
  (undo-limit 67108864)
  (undo-strong-limit 100663296)
  (undo-outer-limit 1006632960))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-compression 'zst)
  :init
  (undo-fu-session-global-mode))

(use-package dumb-jump
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package xref-union
  :config
  ;; TODO: https://codeberg.org/pkal/xref-union.el/issues/1
  (cl-defmethod xref-backend-definitions ((backends (head union)) idents)
    "Collect the results of multiple Xref BACKENDS.
IDENTS is specified in `xref-backend-definitions'."
    (seq-uniq
     (cl-loop for backend in (cdr backends)
              append (xref-backend-definitions backend (alist-get backend idents)))
     #'xref-union-same-p))

  (cl-defmethod xref-backend-identifier-at-point ((backends (head union)))
    "Collect the results of multiple Xref BACKENDS."
    (cl-loop for backend in (cdr backends)
             collect (cons backend (xref-backend-identifier-at-point backend)))))

(use-package multiple-cursors
  :bind-keymap
  (("C-c m" . mc-repeat-map))
  :init
  (defvar-keymap mc-repeat-map
    :repeat t
    ;; Mark next/previous
    ">" #'mc/mark-next-like-this
    "<" #'mc/mark-previous-like-this
    "." #'mc/mark-next-like-this
    "," #'mc/mark-previous-like-this

    ;; Skip cursors
    "s" #'mc/skip-to-next-like-this
    "S" #'mc/skip-to-previous-like-this

    ;; Unmark cursors
    "u" #'mc/unmark-next-like-this
    "U" #'mc/unmark-previous-like-this

    ;; Word/symbol variants
    "w" #'mc/mark-next-like-this-word
    "W" #'mc/mark-previous-like-this-word
    "y" #'mc/mark-next-like-this-symbol
    "Y" #'mc/mark-previous-like-this-symbol

    ;; Mark all variants
    "a" #'mc/mark-all-like-this
    "A" #'mc/mark-all-words-like-this
    "z" #'mc/mark-all-symbols-like-this
    "d" #'mc/mark-all-dwim

    ;; Line operations
    "l" #'mc/edit-lines
    "b" #'mc/edit-beginnings-of-lines
    "e" #'mc/edit-ends-of-lines

    ;; Region operations
    "r" #'mc/mark-all-in-region
    "R" #'set-rectangular-region-anchor

    ;; Utility commands
    "n" #'mc/insert-numbers
    "i" #'mc/insert-letters
    "o" #'mc/sort-regions
    "v" #'mc/reverse-regions
    "=" #'mc/vertical-align
    "+" #'mc/vertical-align-with-space

    ;; HTML/SGML
    "t" #'mc/mark-sgml-tag-pair

    ;; Extended mode
    "x" #'mc/mark-more-like-this-extended))

(use-package indent-bars
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module" "program"))
  (indent-bars-treesit-wrap '((yaml
                               block_mapping_pair comment)
                              (toml
                               table array comment)
                              (lua
                               expression_list function_declaration if_statement
                               elseif_statement else_statement while_statement for_statement
                               repeat_statement comment)
                              (html attribute
                                    start_tag end_tag
                                    quoted_attribute_value)
                              (ruby array
                                    hash
                                    comment
                                    when
                                    argument_list)))
  (indent-bars-no-descend-string t)
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-scope '((html element
                                script_element
                                style_element)
                               (ruby method class module
                                     block
                                     if case unless when
                                     while until for
                                     begin
                                     block)))
  :init
  (defun lkn/treesit-parser-for-lang (lang)
    "Create a parser for LANG if exists."
    (when (and (treesit-available-p)
               (treesit-language-available-p lang))
      (treesit-parser-create lang)))
  :hook
  ((yaml-mode yaml-ts-mode) . indent-bars-mode)
  ((toml-mode toml-ts-mode) . indent-bars-mode)
  ((lua-mode lua-ts-mode) . indent-bars-mode)
  (web-mode . (lambda () (lkn/treesit-parser-for-lang 'html)))
  ((ruby-mode ruby-ts-mode) . indent-bars-mode)
  (fsharp-mode . indent-bars-mode))

(provide 'lkn-edit)
;;; lkn-edit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
