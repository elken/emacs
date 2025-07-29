;;; lkn-macos.el -- Settings for macOS -*- lexical-binding: t -*-
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
;;; I use this a lot for work
;;; Code:

(defcustom lkn-macos-path '("/opt/homebrew/opt/openjdk@11/bin"
                            "/usr/local/opt/coreutils/libexec/gnubin"
                            "/opt/homebrew/opt/coreutils/libexec/gnubin"
                            "/opt/homebrew/opt/openjdk/bin"
                            "~/flutter/sdk/bin"
                            "/opt/homebrew/bin"
                            "/System/Cryptexes/App/usr/bin"
                            "/bin"
                            "/usr/sbin"
                            "/sbin"
                            "/Library/Apple/usr/bin")
  "List of paths to setup to find needed executables."
  :type '(repeat string)
  :set (lambda (var val)
         (set-default var val)
         (setq exec-path (delete-dups (append val exec-path)))
         (setenv "PATH" (string-join exec-path ":")))
  :group 'lkn)

(setq frame-resize-pixelwise t
      ns-use-native-fullscreen nil
      ns-use-proxy-icon nil
      frame-inhibit-implied-resize t
      mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-use-title-bar nil)

(with-eval-after-load 'magit
  (setopt magit-git-executable "/Applications/Xcode.app/Contents/Developer/usr/bin/git"))

(defun lkn/paste-from-osx ()
  "Paste clipboard using pbpaste."
  (shell-command-to-string "pbpaste"))

(defun lkn/copy-to-osx (text &optional _)
  "Used in the terminal to copy TEXT."
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
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
