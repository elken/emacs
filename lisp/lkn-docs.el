;;; lkn-docs.el -- office-lsp when? -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2024Ellis Kenyő

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
;;; Pretty much just a dumping ground for general document/file
;;; packages that don't really belong anywhere else
;;; Code:

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imageamgick nil))

(use-package saveplace-pdf-view
  :after pdf-view)

(provide 'lkn-docs)
;;; lkn-docs.el ends here
