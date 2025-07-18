;;; lkn-project.el -- Project.el tweaks -*- lexical-binding: t -*-
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
;;; Anything related to projects, namely project.el.
;;; Over time it has improved to the point of making projectile redundant now!
;;; Code:

(use-feature project
  :demand t
  :bind
  (:map project-prefix-map
        ("G" . project-gptel))
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-dired "Dired" ?d)
     (project-gptel "gptel" ?g)
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
  (defun project-gptel ()
    "Open a gptel buffer for the current project."
    (interactive)
    (with-current-buffer (gptel (project-root-prefixed-buffer-name "gptel"))
      (display-buffer (current-buffer) gptel-display-buffer-action)))
  :config
  (add-to-list 'project-kill-buffer-conditions  '(major-mode . vterm-mode)))

(provide 'lkn-project)
;;; lkn-project.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
