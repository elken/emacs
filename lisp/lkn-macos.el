;;; lkn-macos.el -- Settings for macOS -*- lexical-binding: t -*-
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
;;; I use this a lot for work
;;; Code:

(setq frame-resize-pixelwise t
      ns-use-native-fullscreen nil
      ns-use-proxy-icon nil
      frame-inhibit-implied-resize t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-use-title-bar nil)

(defun lkn/paste-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun lkn/copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(when (and (not (display-graphic-p))
           (eq system-type 'darwin))
  (setq interprogram-cut-function   #'lkn/copy-to-osx
        interprogram-paste-function #'lkn/paste-from-osx))

(provide 'lkn-macos)
;;; lkn-macos.el ends here
