;;; lkn-terms.el -- Terminal velocity -*- lexical-binding: t -*-
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
;;; Another flavour-of-the-month focused module.
;;; The default terminal options often lack something, so we need to reach for the likes of vterm/eat/etc
;;; Code:

(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :defer t
  :commands
  (lkn/vterm-toggle)
  :custom
  (vterm-shell (executable-find "zsh"))
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  :bind
  ("C-c v" . lkn/vterm-toggle)
  :config
  (define-key vterm-mode-map (kbd "<C-backspace>") (cmd! (vterm-send-key (kbd "C-w"))))
  (add-hook 'vterm-exit-functions
            (cmd!
             (unless (one-window-p)
               (let* ((buf (current-buffer))
                      (win (get-buffer-window buf t)))
                 (when (eq (selected-window) win)
                   (if win
                       (kill-buffer-and-window)
                     (kill-buffer)))))))

  ;; TODO Tidy these up, `add-to-list' maybe?
  (setf (alist-get "woman" vterm-eval-cmds nil nil #'equal)
        '((lambda (topic)
            (woman topic))))
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path))))
  (setf (alist-get "dired" vterm-eval-cmds nil nil #'equal)
        '((lambda (dir)
            (dired dir))))

  ;; Always open vterm buffers on the bottom & default height
  (setf (alist-get "\\*vterm\\*" display-buffer-alist nil nil #'equal)
        '((display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.2)))

  ;; I only ever want 1 per project, if I /really/ need another I can
  ;; manually invoke `vterm'
  ;;;###autoload
  (defun lkn/vterm-toggle ()
  "Toggle a vterm buffer.
Creates or toggles a vterm buffer specific to the current perspective and project."
  (interactive)
  (let* ((project (project-current))
         (buffer-name (if project
                         (project-root-prefixed-buffer-name (format "%s-vterm" (persp-current-name)))
                        (format "*%s-vterm*" (persp-current-name))))
         (default-directory (if project
                              (project-root project)
                            default-directory))
         (buffer (seq-find (lambda (buffer)
                            (and (buffer-live-p buffer)
                                 (string= buffer-name (buffer-name buffer))))
                          (persp-current-buffers))))
    (if buffer
        (if-let (win (get-buffer-window buffer))
            (delete-window win)
          (display-buffer buffer))
      (let ((buffer (generate-new-buffer buffer-name)))
        (with-current-buffer buffer
          (vterm-mode))
        (display-buffer buffer)
        (persp-add-buffer buffer))))))

(provide 'lkn-terms)
;;; lkn-terms.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
