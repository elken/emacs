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
	      use-package-compute-statistics t)

(when IS-MAC
  (setq frame-resize-pixelwise t
        ns-use-native-fullscreen nil
        ns-use-proxy-icon nil
        frame-inhibit-implied-resize t))

;; Get rid of the UI stuff we don't need. Doing it here prevents
;; awkward flashing. Also get rid of the dumb startup message
(set-face-attribute 'fringe nil :background "#000000")
(setq-default
 default-frame-alist
 '((background-color . "#000000")    ; Default background color
   (bottom-divider-width . 1)        ; Thin horizontal window divider
   (foreground-color . "#f4f4f4")    ; Default foreground color
   (fullscreen . (if IS-MAC nil maximized))          ; Maximize the window by default
   (horizontal-scroll-bars . nil)    ; No horizontal scroll-bars
   (left-fringe . 8)                 ; Thin left fringe
   (right-fringe . 8)                ; Thin right fringe
   (right-divider-width . 10)        ; Spacious padding
   (internal-border-width . 0)       ; My WM should add padding
   (menu-bar-lines . 0)                      ; No menu bar
   (tool-bar-lines . 0)                      ; No tool bar
   (undecorated-round . IS-MAC)                      ; Remove extraneous X decorations
   (inhibit-double-buffering  . t)   ; Resolves random flickering
   (alpha-background . 90)        ; Transparency (29+)
   (vertical-scroll-bars . nil))     ; No vertical scroll-bars
 inhibit-startup-message t
 mode-line-format nil)

;; More free real estate tweaks
(setq frame-inhibit-implied-resize t
      native-comp-speed 3
      load-prefer-newer t
      read-process-output-max (* 1024 1024)
      large-file-warning-threshold (* 100 1024 1024)
      gc-cons-threshold (* 50 1000 1000)
      native-comp-jit-compilation t
      native-comp-async-report-warnings-errors nil
      require-final-newline t)

;; Redirect the eln-cache into our no-littering structure
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  ".local/var/eln-cache/" user-emacs-directory))))

;; Inspired by <https://github.com/daviwil/emacs-from-scratch/blob/d23348b4a52dde97f4f7cbcd66a519b5fd0a143c/init.el#L14-L19>
;; Compute and print the startup time for Emacs after loading
(defun display-startup-time ()
  (message "Emacs loaded %d packages in %s with %d garbage collections."
	   (length (mapcar #'car (elpaca--queued)))
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))


(add-hook 'elpaca-after-init-hook #'display-startup-time)
