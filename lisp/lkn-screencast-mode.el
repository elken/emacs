;;; lkn-screencast-mode.el -- A simple mode for screensharing -*- lexical-binding: t -*-
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
;;; A mode for screensharing to enable some useful things
;;; Code:

(use-package keycast
  :commands (keycast-tab-bar-mode))

(use-package fontaine
  :commands (fontaine-set-preset)
  :custom
  (fontaine-presets
   `((regular
      :default-family ,lkn-default-font
      :default-height ,lkn-default-font-height
      :fixed-pitch-family ,lkn-default-font
      :fixed-pitch-height ,lkn-default-font-height
      :variable-pitch-family ,lkn-variable-pitch-font)
     (screencast
      :default-family ,lkn-default-font
      :default-height 220
      :fixed-pitch-family ,lkn-default-font
      :fixed-pitch-height 220
      :variable-pitch-family ,lkn-variable-pitch-font)))
  :config
  ;; Set initial preset
  (fontaine-set-preset 'regular))

;;;###autoload
(define-minor-mode lkn-screencast-mode
  "Minor mode for screencasting."
  :global t
  :lighter " Screencast"
  (if lkn-screencast-mode
      (progn
        (fontaine-set-preset 'screencast)
        (keycast-tab-bar-mode 1))
    (fontaine-set-preset 'regular)
    (keycast-tab-bar-mode -1)))

(provide 'lkn-screencast-mode)
;;; lkn-screencast-mode.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
