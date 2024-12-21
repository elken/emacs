;;; lkn-vcs.el -- saved_file.backup.old.sav (2) -*- lexical-binding: t -*-
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

;;; Commentary:
;;; Git is the only VCS I use, but to save me ever having to rename this I'll make it generic
;;; Code:


(use-package magit
  :custom
  (magit-auto-revert-mode nil)
  :hook
  (magit-mode . lkn/keychain-setup)
  :init
  ;; Borrowed from Doom
  (defun display-buffer-full-frame-restore (buffer alist)
  "Display BUFFER in full frame and restore the layout when done."
  (if-let ((window (get-buffer-window buffer)))
      window  ; If buffer is already displayed, just return its window
    (let ((conf (current-window-configuration)))
      (delete-other-windows)
      (with-current-buffer buffer
        (setq-local quit-restore-window nil)
        (add-hook 'kill-buffer-hook
                  (lambda () (set-window-configuration conf))
                  nil t))
      (display-buffer-full-frame buffer alist))))
  
;;;###autoload
  (defun magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...

- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
		'(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
		(let ((size (if (eq buffer-mode 'magit-process-mode)
				0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))
               ((eq buffer-mode 'magit-status-mode)
		'(display-buffer-full-frame-restore))
             
               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
				 magit-revision-mode
				 magit-diff-mode
				 magit-stash-mode))))
		'(display-buffer-same-window))

               ('(magit--display-buffer-in-direction))))))

  (defun magit--display-buffer-in-direction (buffer alist)
    "`display-buffer-alist' handler that opens BUFFER in a direction.

This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
    (let ((direction (or (alist-get 'direction alist)
			 'right))
          (origin-window (selected-window)))
      (if-let (window (window-in-direction direction))
          (unless magit-display-buffer-noselect
            (select-window window))
	(if-let (window (and (not (one-window-p))
                             (window-in-direction
                              (pcase direction
				(`right 'left)
				(`left 'right)
				((or `up `above) 'down)
				((or `down `below) 'up)))))
            (unless magit-display-buffer-noselect
              (select-window window))
          (let ((window (split-window nil nil direction)))
            (when (and (not magit-display-buffer-noselect)
                       (memq direction '(right down below)))
              (select-window window))
            (display-buffer-record-window 'reuse window buffer)
            (set-window-buffer window buffer)
            (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
            (set-window-prev-buffers window nil))))
      (unless magit-display-buffer-noselect
	(switch-to-buffer buffer t t)
	(selected-window))))

  :config
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration)

  (defvar magit-stale-p nil)

  (defun magit-revert-buffer (buffer)
    "Revert the current buffer and update the modeline."
    (with-current-buffer buffer
      (kill-local-variable 'magit-stale-p)
      (when (and buffer-file-name (file-exists-p buffer-file-name))
	(if (buffer-modified-p (current-buffer))
	    (when (bound-and-true-p vc-mode)
	      (vc-refresh-state)
	      (force-mode-line-update))
	  (revert-buffer t t t)))))
  
  (defun magit-mark-stale-buffers-a (&rest _)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
	(if (get-buffer-window buffer)
	    (magit-revert-buffer buffer)
	  (with-current-buffer buffer
	    (setq-local magit-stale-p t))))))

  (defun magit-revert-buffer-maybe-h ()
    "Revert the buffer if out of date."
    (when magit-stale-p
      (magit-revert-buffer (current-buffer))))

  (defun magit-set-git-executable-h ()
    (when-let (path (executable-find magit-git-executable t))
      (setq-local magit-git-executable path)))

  (add-hook 'buffer-list-update-hook #'magit-revert-buffer-maybe-h)
  (add-hook 'magit-status-mode-hook #'magit-set-git-executable-h)
  (advice-add 'magit-checkout :after #'magit-mark-stale-buffers-a)
  (advice-add 'magit-branch-and-checkout :after #'magit-mark-stale-buffers-a))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package magit-file-icons
  :after magit
  :init
  (magit-file-icons-mode 1))

(use-package forge
  :after magit
  :preface
  (setq forge-database-file (lkn/cache-dir "forge/forge-database.sqlite")
        forge-add-default-bindings nil
        forge-owned-accounts '(("elken")))
  ;; :bind
  ;; ((:map forge-topic-list-mode-map
  ;;        ("q" . kill-current-buffer))
  ;;  (:map magit-mode-map
  ;;        ([remap magit-browse-thing] . forge-browse-dwim))
  ;;  (:map magit-remote-section-map
  ;;        ([remap magit-browse-thing] . forge-browse-remote))
  ;;  (:map magit-branch-section-map
  ;;        ([remap magit-browse-thing] . forge-browse-branch)))
  )

(use-package code-review
  :ensure (:host github :repo "doomelpa/code-review")
  :after (magit forge)
  :preface
  (setq code-review-db-database-file (lkn/cache-dir "code-review/code-review-db-file.sqlite")
        code-review-log-file (lkn/cache-dir "code-review/code-review-error.log")
        code-review-download-dir (lkn/cache-dir "code-review/"))
  :bind
  (:map code-review-mode-map
	("r" . code-review-transient-api)
	("RET" . code-review-comment-add-or-edit)))

(use-package magit-delta
  :after magit
  :disabled t
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-delta-args
	'("--true-color" "always"
	  "--features" "magit-delta"
	  "--syntax-theme" "Nord"
	  "--dark"
	  "--color-only")))

(use-package transient
  :bind
  (:map transient-map
        ([escape] . transient-quit-one))
  :init
  (setq transient-default-level 5
	magit-diff-refine-hunk t
	magit-save-repository-buffers nil
	magit-revision-insert-related-refs nil
	transient-levels-file  (expand-file-name "transient/levels" no-littering-var-directory)
        transient-values-file  (expand-file-name "transient/values" no-littering-var-directory)
        transient-history-file (expand-file-name "transient/history" no-littering-var-directory)
        transient-default-level 5))

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :init
  (global-diff-hl-mode))

(use-package browse-at-remote
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind
  (:map global-map
	("C-c g y" . browse-at-remote-kill)))

(provide 'lkn-vcs)
;;; lkn-vcs.el ends here
