;;; lkn-popup-frame.el --- Popup frame utilities -*- lexical-binding: t -*-
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
;; Utilities for creating and managing small popup frames that execute commands
;; and return focus to the previous application or frame.

;;; Code:

(defun popup-frame-delete (&rest _)
  "Kill selected frame if it has parameter `popup-frame'."
  (let ((frame (seq-find (lambda (f) (frame-parameter f 'popup-frame)) (frame-list))))
    (when frame
      (let ((other-frame (seq-find (lambda (f) (not (frame-parameter f 'popup-frame))) (frame-list))))
        (if other-frame
            (select-frame-set-input-focus other-frame)
          (when-let* ((bundle (frame-parameter frame 'bundle-identifier)))
            (condition-case _ (ns-do-applescript (format "tell application id \"%s\" to activate" bundle)) (error nil)))))
      (delete-frame frame))))

(defmacro popup-frame-define (command)
  "Define interactive function to call COMMAND in frame with TITLE."
  `(defun ,(intern (format "popup-frame-%s" command)) ()
     (interactive)
     (let* ((bundle-identifier (when IS-MAC
                                 (string-trim (shell-command-to-string "lsappinfo info $(lsappinfo front) | grep bundleID | sed 's/.*bundleID=\"\\([^\"]*\\)\".*/\\1/'"))))
            (original-default-frame-alist default-frame-alist)
            (frame (make-frame
                    `((title . ,(format "popup-frame-%s" ',command))
                      (height . 25)
                      (width . 120)
                      (fullscreen . nil)
                      (minibuffer . only)
                      (popup-frame . t)
                      (bundle-identifier . ,bundle-identifier)))))
       (select-frame-set-input-focus frame)
       (hide-mode-line-mode)
       (unwind-protect
           (condition-case nil
               (let ((default-frame-alist (cons '(hide-mode-line . nil) (cons '(fullscreen . maximized) original-default-frame-alist))))
                 (call-interactively ',command)
                 (delete-other-windows)
                 (popup-frame-delete))
             ((quit error user-error)
              (progn
                (when bundle-identifier
                  (ns-do-applescript (format "tell application id \"%s\" to activate" bundle-identifier)))
                (delete-frame frame))))
         (setq default-frame-alist original-default-frame-alist)))))

(provide 'lkn-popup-frame)
;;; lkn-popup-frame.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
