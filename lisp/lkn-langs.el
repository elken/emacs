;;; lkn-langs.el -- Another dumping ground -*- lexical-binding: t -*-
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
;;; Languages that don't have enough config to warrant an entire file
;;; Code:


(use-package dockerfile-mode
  :custom
  (dockerfile-mode-command (if IS-LINUX "podman" "docker")))

(use-package yaml-mode
  :mode ".snyk")

(use-package k8s-mode)

(use-package web-mode
  :mode "\\.html.erb\\'")

(provide 'lkn-langs)
;;; lkn-langs.el ends here
