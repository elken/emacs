;;; lkn-langs.el -- Another dumping ground -*- lexical-binding: t -*-
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
;;; Languages that don't have enough config to warrant an entire file
;;; Code:


(use-package dockerfile-mode
  :custom
  (dockerfile-mode-command (if IS-LINUX "podman" "docker")))

(use-package yaml-mode
  :mode ".snyk")

(use-package k8s-mode)

(use-package web-mode
  :custom
  (web-mode-auto-close-style 2)
  :mode "\\.html.erb\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :custom
  (markdown-fontify-code-block-natively t))

(use-package csv-mode
  :commands (csv-align-mode))

(use-package fsharp-mode
  :mode "\\.fs\\'")

(use-package eglot-fsharp
  :after fsharp-mode)

(use-package haskell-ts-mode
  :mode "\\.hs\\'"
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  (haskell-ts-use-indent t)
  :config
  (add-to-list 'treesit-language-source-alist
   '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
   (treesit-install-language-grammar 'haskell)))

(provide 'lkn-langs)
;;; lkn-langs.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
