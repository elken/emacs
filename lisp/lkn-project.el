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
  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-dired "Dired" ?d)
     (magit-project-status "Magit" ?m)
     (consult-ripgrep "Search" ?s)
     (tabspaces-restore-session "Restore Session" ?r)))
  (project-compilation-buffer-name-function #'project-root-prefixed-buffer-name)
  :init
  (defun project-shell-command (command)
    "Run COMMAND in project root and show result in minibuffer."
    (interactive (list (read-shell-command "Project command: ")))
    (let* ((project (project-current t))
           (default-directory (project-root project))
           (cmd (split-string-and-unquote command)))
      (message "%s" (string-trim
                     (inheritenv
                      (with-temp-buffer
                        (apply 'call-process (car cmd) nil t nil (cdr cmd))
                        (buffer-string)))))))

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
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
