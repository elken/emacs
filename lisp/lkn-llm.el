;;; lkn-llm.el -- My grandma used to tell me stories about generating code -*- lexical-binding: t -*-
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
;;; Pretty much just config for gptel, but may also include more
;;; packages over time.
;;; Code:

(use-package gptel
  :hook
  (gptel-post-stream . corfu-quit)
  (gptel-post-stream . gptel-auto-scroll)
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (setq-hook! 'gptel-mode-hook
    markdown-hide-markup t
    markdown-fontify-code-blocks-natively t)

  (setopt
   gptel-model 'claude-3-sonnet-20240229
   gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key #'gptel-api-key-from-auth-source)))

(provide 'lkn-llm)
;;; lkn-llm.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
