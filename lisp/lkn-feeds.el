;;; lkn-feeds.el -- What a tangled web we weave -*- lexical-binding: t -*-
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
;;; Anything relating to news feeds of various kinds.
;;; Basically just elfeed setup
;;; Code:

(use-package elfeed
  :custom
  (elfeed-feeds
   '(("http://nullprogram.com/feed/" emacs)
     ("https://planet.emacslife.com/atom.xml" emacs)
     ("https://sachachua.com/blog/category/emacs-news/feed/atom/index.xml" emacs)))
  :bind
  (:map elfeed-search-mode-map
        ("q" . delete-frame))
  ("C-x w" . elfeed))

(use-package elfeed-goodies
  :after elfeed
  :custom
  (elfeed-goodies/entry-pane-position 'bottom)
  :init
  (defun elfeed-goodies/show-mode-setup ()
    "Setup function providing defaults for show mode buffer."
    (setq header-line-format '(:eval (elfeed-goodies/entry-header-line))
          left-margin-width elfeed-goodies/show-mode-padding
          right-margin-width elfeed-goodies/show-mode-padding))
  :config
  (elfeed-goodies/setup))

(provide 'lkn-feeds)
;;; lkn-feeds.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
