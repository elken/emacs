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
  :bind (("C-c v" . multi-vterm-project)
         ("C-c t" . lkn/multi-vterm-project)
         ("C-c d" . multi-vterm-dedicated-toggle)
         ("C-c V" . lkn/consult-project-vterm))
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
    (setf (alist-get (car cmd) vterm-eval-cmds nil nil #'equal) (list (cdr cmd)))))

(use-package multi-vterm
  :after vterm
  :config
;;;###autoload
  (defun lkn/vterm-preview-state ()
    "State function for buffer switcher to display preview either in the
popper popup or similarly small window."
    (let (preview-window last-buffer)
      (lambda (action cand)
        (cond
         ((and cand (eq action 'preview))
          (when-let* ((buffer (get-buffer cand)))
            ;; Check if there's already a popup window visible
            (if-let* ((popup-win (cl-find-if (lambda (win)
                                              (and (window-live-p win)
                                                   (with-current-buffer (window-buffer win)
                                                     (eq major-mode 'vterm-mode))))
                                            (window-list))))
                ;; Use existing popup window
                (progn
                  (set-window-buffer popup-win buffer)
                  (unless (eq buffer last-buffer)
                    (with-current-buffer buffer
                      (pulse-momentary-highlight-region (point-min) (point-max)))
                    (setq last-buffer buffer)))
              ;; Create temporary preview window
              (unless (window-live-p preview-window)
                (setq preview-window (split-window nil (floor (* (window-height) -0.3)) 'below)))
              (set-window-buffer preview-window buffer)
              (unless (eq buffer last-buffer)
                (with-current-buffer buffer
                  (pulse-momentary-highlight-region (point-min) (point-max)))
                (setq last-buffer buffer)))))
         ((eq action 'exit)
          (when (window-live-p preview-window)
            (delete-window preview-window)))))))

;;;###autoload
  (defun lkn/multi-vterm-project ()
    "Create a new multi-vterm in a way that pleases Popper"
    (interactive)
    (when-let* ((project (project-current)))
      (let* ((default-directory (project-root project))
             (vterm-buffer (multi-vterm-get-buffer)))
        (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
        (set-buffer vterm-buffer)
        (multi-vterm-internal)
        (pop-to-buffer vterm-buffer))))

;;;###autoload
  (defun lkn/consult-project-vterm ()
    "Switch to a project's vterm buffer using Consult."
    (interactive)
    (when-let* ((project (project-current)))
      (let ((vterm-buffers (seq-filter
                            (lambda (buf)
                              (with-current-buffer buf
                                (eq major-mode 'vterm-mode)))
                            (persp-current-buffers))))
        (if vterm-buffers
            (let ((selected-buffer (get-buffer
                                    (consult--read (mapcar #'buffer-name vterm-buffers)
                                                   :prompt "Project vterm: "
                                                   :category 'buffer
                                                   :require-match t
                                                   :state (lkn/vterm-preview-state)
                                                   :preview-key (list :debounce 0.3 'any)
                                                   :sort nil))))
              (pop-to-buffer selected-buffer))
          (message "No project vterm buffers found. Creating one...")
          (lkn/multi-vterm-project))))))

(provide 'lkn-terms)
;;; lkn-terms.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
