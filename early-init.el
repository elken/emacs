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

;; No need to enable packages here yet.
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq-default package-user-dir (expand-file-name "packages/" LITTER-DIR)
              package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
              use-package-always-ensure t
              use-package-compute-statistics t
              use-package-always-defer t
              package-quickstart t)

;; Need to set this so we can definitely find libgccjit
(when IS-MAC
  (setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH")))
  (setenv "LIBRARY_PATH" (concat "/opt/homebrew/lib:" (getenv "LIBRARY_PATH")))
  (setenv "LD_LIBRARY_PATH" (concat "/opt/homebrew/lib:" (getenv "LD_LIBRARY_PATH")))
  (setq native-comp-compiler-options '("-mcpu=apple-m1" "-O3" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto")
        native-comp-driver-options '("-mcpu=apple-m1" "-O3" "-g0" "-fno-finite-math-only" "-fgraphite-identity" "-floop-nest-optimize" "-fdevirtualize-at-ltrans" "-fipa-pta" "-fno-semantic-interposition" "-flto=auto")))


;; Get rid of the UI stuff we don't need. Doing it here prevents
;; awkward flashing. Also get rid of the dumb startup message
(set-face-attribute 'fringe nil :background "#2E3440" :foreground "#ECEFF4")

(setq-default
 default-frame-alist
 `((foreground-color . "#ECEFF4")
   (background-color         . "#2E3440")
   (fullscreen               . ,'maximized) ; Maximize the window by default
   (horizontal-scroll-bars   . nil)         ; No horizontal scroll-bars
   (left-fringe              . 8)           ; Thin left fringe
   (right-fringe             . 8)           ; Thin right fringe
   (menu-bar-lines           . 0)           ; No menu bar
   (tool-bar-lines           . 0)           ; No tool bar
   (ns-transparent-titlebar  . IS-MAC)      ; Make titlebar same colour as background
   (undecorated-round        . IS-MAC)      ; Remove extraneous X decorations
   (inhibit-double-buffering . t)           ; Resolves random flickering
   (vertical-scroll-bars     . nil)))       ; No vertical scroll-bars

;; More free real estate tweaks
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t
      initial-scratch-message nil
      cursor-in-non-selected-windows nil
      mode-line-format nil
      auto-save-default nil
      backup-by-copying t
      inhibit-compacting-font-caches t
      bidi-inhibit-bpa t
      custom-theme-directory (expand-file-name "themes" user-emacs-directory)
      frame-inhibit-implied-resize t
      native-comp-speed 3
      load-prefer-newer t
      read-process-output-max (* 1024 1024)
      large-file-warning-threshold (* 100 1024 1024)
      gc-cons-threshold 100000000
      gc-cons-percentage 0.6
      native-comp-jit-compilation t
      native-comp-async-report-warnings-errors 'silent
      native-comp-deferred-compilation t
      require-final-newline t)

;; Redirect the eln-cache into our no-littering structure
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  ".local/var/eln-cache/" user-emacs-directory))))

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
