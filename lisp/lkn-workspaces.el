;;; lkn-workspaces.el -- Mangaging loads of frames and buffers sucks -*- lexical-binding: t -*-
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
;;; I work a lot with a couple of projects at once, and managing all
;;; these buffers at once isn't fun.
;;; Code:

(use-package beframe
  :when IS-MAC
  :init (beframe-mode)
  :custom (beframe-functions-in-frames '(project-prompt-project-dir
                                         elfeed))
  :bind (("C-x f"   . other-frame-prefix)
         ("C-x b"   . beframe-switch-buffer)
         ("C-x C-b" . beframe-buffer-menu)))

(use-package tabspaces
  :disabled IS-MAC
  :hook (after-init . tabspaces-mode)
  :bind
  (:map project-prefix-map
        ("p" . tabspaces-open-or-create-project-and-workspace))
  :custom
  (tabspaces-echo-area-enable nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(provide 'lkn-workspaces)
;;; lkn-workspaces.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
