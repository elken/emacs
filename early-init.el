;;; early-init.el --- lknmacs -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2023 Ellis Kenyő

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
;;
;;; Commentary:
;;;  Do fancy things here (but _faster_)
;;; Code:

;; OS Checks
(defconst IS-MAC        (eq system-type 'darwin))
(defconst IS-LINUX      (eq system-type 'gnu/linux))
(defconst IS-WINDOWS    (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD        (or IS-MAC (eq system-type 'berkeley-unix)))

;; no-littering
(defconst LITTER-DIR    (expand-file-name ".local/" user-emacs-directory))
(defconst CACHE-DIR     (expand-file-name "cache/" LITTER-DIR))

;; Package management setup
(setq-default package-enable-at-startup nil
              package-user-dir (expand-file-name "packages/" LITTER-DIR)
              package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
              use-package-always-ensure t
              use-package-compute-statistics t
              package-quickstart t)

;; Need to set this so we can definitely find libgccjit
(when IS-MAC
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (setenv "LIBRARY_PATH" (concat "/opt/homebrew/lib:" (getenv "LIBRARY_PATH")))
  (setenv "LD_LIBRARY_PATH" (concat "/opt/homebrew/lib:" (getenv "LD_LIBRARY_PATH")))
  (setq native-comp-compiler-options '("-mcpu=apple-m1" "-O3" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto")
        native-comp-driver-options native-comp-compiler-options))


;; Get rid of the UI stuff we don't need. Doing it here prevents
;; awkward flashing. Also get rid of the dumb startup message
(set-face-attribute 'fringe nil :background "#2E3440" :foreground "#ECEFF4")

(setq-default
 default-frame-alist
 `((foreground-color         . "#ECEFF4") ; Theme-based foreground (to prevent ugly paints)
   (background-color         . "#2E3440") ; Theme-based background (to prevent ugly paints)
   (fullscreen               . maximized) ; Maximize the window by default
   (horizontal-scroll-bars   . nil)       ; No horizontal scroll-bars
   (left-fringe              . 8)         ; Thin left fringe
   (right-fringe             . 8)         ; Thin right fringe
   (menu-bar-lines           . 0)         ; No menu bar
   (tool-bar-lines           . 0)         ; No tool bar
   (ns-transparent-titlebar  . IS-MAC)    ; Make titlebar same colour as background
   (undecorated-round        . IS-MAC)    ; Remove extraneous X decorations
   (inhibit-double-buffering . t)         ; Resolves random flickering
   (vertical-scroll-bars     . nil)))     ; No vertical scroll-bars

;; More free real estate tweaks
(setq inhibit-startup-message t                        ; Don't show GNU Emacs startup message
      inhibit-startup-echo-area-message t              ; Don't show startup echo area message
      inhibit-startup-screen t                         ; Don't show the startup screen
      initial-scratch-message nil                      ; Empty *scratch* buffer on startup
      cursor-in-non-selected-windows nil               ; Hide cursor in inactive windows
      mode-line-format nil                             ; Disable mode-line (set properly later)
      auto-save-default nil                            ; Disable auto-save (handled by no-littering)
      backup-by-copying t                              ; Copy files when backing up (safer for symlinks)
      inhibit-compacting-font-caches t                 ; Don't compact font caches (speeds up display)
      bidi-inhibit-bpa t                               ; Disable bidirectional parentheses (faster redisplay)
      frame-inhibit-implied-resize t                   ; Don't resize frame implicitly (faster startup)
      native-comp-speed 3                              ; Maximum native compilation optimization level
      load-prefer-newer t                              ; Load newer .el files over older .elc files
      read-process-output-max (* 1024 1024)            ; 1MB read buffer for subprocesses (LSP performance)
      large-file-warning-threshold (* 100 1024 1024)   ; Warn for files larger than 100MB
      gc-cons-threshold 100000000                      ; 100MB GC threshold (reduces GC during startup)
      gc-cons-percentage 0.6                           ; GC when 60% of memory is allocated
      native-comp-jit-compilation t                    ; Enable JIT compilation for native code
      native-comp-async-report-warnings-errors 'silent ; Suppress native-comp warnings
      native-comp-deferred-compilation t               ; Automatically compile .el files when loaded
      require-final-newline t)                         ; Always add newline at end of file

;; Set the custom theme directory early so we can load it properly later
(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

;; Inspired by <https://github.com/daviwil/emacs-from-scratch/blob/d23348b4a52dde97f4f7cbcd66a519b5fd0a143c/init.el#L14-L19>
;; Compute and print the startup time for Emacs after loading
(defun display-startup-time ()
  "Print startup time after loading."
  (message "Emacs loaded %d packages in %s with %d garbage collections."
           (length (mapcar #'car (elpaca--queued)))
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))


(add-hook 'elpaca-after-init-hook #'display-startup-time)

(provide 'early-init)
;;; early-init.el ends here
