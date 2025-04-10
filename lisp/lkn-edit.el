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
  :config
  (with-eval-after-load 'meow
    (defvar meow--last-avy-char)

    (defun meow-avy-goto-char (char &optional arg expand)
      "Goto using avy"
      (interactive (list (read-char "goto: " t)
                         current-prefix-arg))
      (let* ((beg (point))
             (end (save-mark-and-excursion
                    (avy-goto-char char arg)
                    (point))))
        (thread-first
          (meow--make-selection '(select . avy)
                                beg end expand)
          (meow--select)))
      (setq meow--last-avy-char char))

    (defun meow-avy-goto-char-expand (char &optional arg)
      "Goto using avy expand"
      (interactive (list (read-char "Expand goto: " t)
                         current-prefix-arg))
      (meow-avy-goto-char char arg t))

    (defun meow--add-beacons-for-avy ()
      "Add beacon for avy movement"
      (let ((ch-str (if (eq meow--last-avy-char 13)
                        "\n"
                      (char-to-string meow--last-avy-char))))
        (save-restriction
          (meow--narrow-secondary-selection)
          (let ((orig (point))
                (case-fold-search t))
            (save-mark-and-excursion
              (goto-char (point-max))
              (while (search-backward ch-str nil t)
                (unless (= (point) orig)
                  (meow--beacon-add-overlay-at-point (point)))))))
        (meow--beacon-shrink-selection)))

    (defun meow--beacon-update-overlays-custom ()
      (when (meow--beacon-inside-secondary-selection)
        (let* ((ex (car (meow--selection-type)))
               (type (cdr (meow--selection-type))))
          (cl-case type
            ((avy) (meow--add-beacons-for-avy))))))

    (advice-add 'meow--beacon-update-overlays :after #'meow--beacon-update-overlays-custom)))

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
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))

  (add-to-list 'tempel-user-elements #'lkn-snippets-include))

(use-package treesit-auto
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

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate" :branch "development" :files (:defaults "tests/*.el"))
  :when (treesit-available-p)
  :hook ((ruby-mode . combobulate-mode))
  :config
  (load (expand-file-name "combobulate/tests/combobulate-test-prelude.el" elpaca-repos-directory) t)
  (require 'combobulate-debug))

(use-package expreg
  :when (treesit-available-p))

(use-package undo-fu
  :bind
  (("C-/" . undo-fu-only-undo)
   ([remap undo-redo] . undo-fu-only-redo))
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

(use-package smartscan
  :init
  (global-smartscan-mode))

(provide 'lkn-edit)
;;; lkn-edit.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
