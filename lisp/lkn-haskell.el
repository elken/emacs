;;; lkn-haskell.el -- Scotland did something right for once -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2025 Ellis Kenyő <emacs@lkn.mozmail.com>

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

;;; Any and all configuration related to Haskell.  I'm far from an
;;; expert, I've just started tinkering around with it

;;; Code:

(use-package haskell-mode
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-literate-mode)
         ("\\.hsc\\'" . haskell-mode)
         ("\\.cpphs\\'" . haskell-mode)
         ("\\.c2hs\\'" . haskell-mode))

  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . prettify-symbols-mode)
         (haskell-mode . lkn/haskell-prettify-symbols))

  :bind (:map haskell-mode-map
         ("C-c C-l" . haskell-process-load-or-reload)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-compile)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         ("M-." . haskell-mode-jump-to-def-or-tag)
         ("<f8>" . haskell-navigate-imports)
         ("C-c C-," . haskell-mode-format-imports)
         ("C-c C-s" . haskell-mode-toggle-scc-at-point))

  :bind (:map haskell-cabal-mode-map
         ("C-`" . haskell-interactive-bring)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-c" . haskell-compile)
         ("C-c c" . haskell-process-cabal))

  :custom
  ;; Process configuration
  (haskell-process-type 'auto)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)

  ;; Compilation
  (haskell-compiler-type 'auto)

  ;; Indentation (default to haskell-indentation-mode)
  (haskell-indentation-electric-flag t)
  :init
  (defun lkn/haskell-prettify-symbols ()
    (setq-local prettify-symbols-alist
                '(;; Lambda and basic operators
                  ("\\" . "λ")
                  ("/=" . "≠")
                  ("->" . "→")
                  ("=>" . "⇒")
                  ("<-" . "←")
                  ("<=" . "≤")
                  (">=" . "≥")
                  ("/<" . "≮")
                  ("/>" . "≯")
                  ("==" . "≡")
                  ("&&" . "∧")
                  ("||" . "∨")

                  ;; Function composition and application
                  (">>=" . "»=")
                  (">>" . "»")
                  ("=<<" . "=«")
                  ("<<" . "«")

                  ;; Type operators
                  ("::" . "∷")

                  ;; Set theory and collections
                  ("forall" . "∀")
                  ("exists" . "∃")
                  ("elem" . "∈")
                  ("notElem" . "∉")
                  ("member" . "∈")
                  ("notMember" . "∉")
                  ("union" . "∪")
                  ("intersection" . "∩")
                  ("isSubsetOf" . "⊆")
                  ("isProperSubsetOf" . "⊂")
                  ("mempty" . "∅")
                  ("empty" . "∅")

                  ;; Math functions
                  ("sum" . "∑")
                  ("product" . "∏")
                  ("sqrt" . "√")
                  ("pi" . "π")
                  ("alpha" . "α")
                  ("beta" . "β")
                  ("gamma" . "γ")
                  ("delta" . "δ")

                  ;; Common keywords
                  ("undefined" . "⊥")
                  ("bottom" . "⊥")
                  ("not" . "¬")
                  ("infinity" . "∞")
                  ("Nothing" . "∅")
                  ("Just" . "⦿")))))

(use-package flymake-hlint
  :hook ((haskell-mode haskell-ts-mode) . flymake-hlint-load))

(provide 'lkn-haskell)
;;; lkn-haskell.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
