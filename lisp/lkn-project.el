;;; lkn-project.el -- Project.el tweaks -*- lexical-binding: t -*-
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
;;; Anything related to projects, namely project.el.
;;; Over time it has improved to the point of making projectile redundant now!
;;; Code:

(use-package project
  :ensure nil
  :demand t
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (lkn/vterm-toggle "Terminal" ?t)
     (magit-project-status "Magit" ?m)
     (consult-ripgrep "Search" ?s)))
  (project-compilation-buffer-name-function #'project-root-prefixed-buffer-name)
  :init
  (defun project-root-prefixed-buffer-name (mode)
    (concat "*"
            (project-name (project-current))
            "-"
            (downcase mode)
            "*"))
  :config
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode)))

(provide 'lkn-project)
;;; lkn-project.el ends here
