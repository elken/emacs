;;; lkn-cloud.el -- Living on cloud9.io -*- lexical-binding: t -*-
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
;;; A collection of packages used across the various cloud workflows
;;; Code:


(use-package k8s-mode)

(use-package terraform-mode
  :after eglot
  :hook
  (terraform-mode . eglot-ensure)
  :custom
  (terraform-indent-level 2)
  :config
  (eglot-set-server (terraform-mode terraform-ts-mode) '("terraform-lsp" "-enable-log-file")))

(provide 'lkn-cloud)
;;; lkn-cloud.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
