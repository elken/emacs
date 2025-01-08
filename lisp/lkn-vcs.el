;;; lkn-vcs.el -- saved_file.backup.old.sav (2) -*- lexical-binding: t -*-
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
;;; Git is the only VCS I use, but to save me ever having to rename this I'll make it generic
;;; Code:


(use-package magit
  :custom
  (magit-refresh-status-buffer nil)
  (magit-git-global-arguments
      '("--no-pager"
        "--literal-pathspecs"
        "-c" "core.preloadindex=true"
        "-c" "gc.auto=0"
        "-c" "core.commitGraph=true"))
  (magit-git-executable (executable-find "git"))
  :bind
  (:map magit-status-mode-map
        ("@" . forge-dispatch))
  :hook
  (magit-mode . lkn/keychain-setup)
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

  (add-hook 'buffer-list-update-hook #'magit-revert-buffer-maybe-h)
  (advice-add 'magit-checkout :after #'magit-mark-stale-buffers-a)
  (advice-add 'magit-branch-and-checkout :after #'magit-mark-stale-buffers-a)

  ;; Taken from https://emacs.stackexchange.com/questions/19440/magit-extremely-slow-in-windows-how-do-i-optimize
  (defvar-local magit-git--cache
      (make-hash-table :test 'equal)
    "Cache for git command results.")

  (defun magit-git--memoize (fn &rest args)
    "Memoize results of git commands for FN with ARGS."
    (let* ((key (cons default-directory (cons fn args)))
           (cached-val (gethash key magit-git--cache 'not-found)))
      (if (eq cached-val 'not-found)
          (puthash key (apply fn args) magit-git--cache)
        cached-val)))

  (dolist (fn '(magit-rev-parse-safe magit-get magit-get-boolean))
    (advice-add fn :around #'magit-git--memoize)))

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
  :config
  (remove-hook 'forge-post-mode-hook 'turn-on-flyspell)
  :bind
  ((:map forge-topics-mode-map
         ("q" . kill-current-buffer))
   (:map magit-mode-map
         ([remap magit-browse-thing] . forge-browse-dwim))
   (:map magit-remote-section-map
         ([remap magit-browse-thing] . forge-browse-remote))
   (:map magit-branch-section-map
         ([remap magit-browse-thing] . forge-browse-branch))))

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
  :custom
  (diff-hl-update-async t)
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
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
