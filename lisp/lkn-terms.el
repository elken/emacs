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
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :bind
  (:map project-prefix-map
        ("v" . lkn/vterm-toggle))
  :custom
  (vterm-shell (executable-find "zsh"))
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  :config
  (define-key vterm-mode-map (kbd "<C-backspace>") (cmd! (vterm-send-key (kbd "C-w"))))
  (add-hook 'vterm-exit-functions
            (cmd!
             (when (and (not (one-window-p)) (eq (selected-window) (get-buffer-window (current-buffer) t)))
               (kill-buffer-and-window))))

  (dolist (cmd '(("woman" . (lambda (topic) (woman topic)))
                 ("magit-status" . (lambda (path) (magit-status path)))
                 ("dired" . (lambda (dir) (dired dir)))))
    (setf (alist-get (car cmd) vterm-eval-cmds nil nil #'equal) (list (cdr cmd))))

;;;###autoload
  (defun lkn/vterm-toggle ()
    "Toggle a vterm buffer.
Creates or toggles a vterm buffer specific to the current perspective and project."
    (interactive)
    (let* ((project (project-current))
           (buffer-name (format "*vterm @ %s*" (lkn/current-perspective-name)))
           (default-directory (if project (project-root project) default-directory))
           (buffer (get-buffer buffer-name)))
      (if-let ((win (and buffer (get-buffer-window buffer))))
          (delete-window win)
        (unless buffer
          (setq buffer (vterm buffer-name))
          (with-current-buffer buffer
            (display-line-numbers-mode -1)
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when-let ((window (get-buffer-window (current-buffer))))
                          (ignore-errors (delete-window window))))
                      t t)))
        (display-buffer-in-side-window
         buffer
         '((side . bottom)
           (slot . 0)
           (window-width . 0.3)
           (preserve-size . (t . nil))
           (dedicated . t)
           (window-parameters . ((no-delete-other-windows . t)
                                 (mode-line-format . none)))))
        (select-window (get-buffer-window buffer))))))

(provide 'lkn-terms)
;;; lkn-terms.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
