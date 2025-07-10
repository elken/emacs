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

(use-package with-editor
  :hook (after-init . shell-command-with-editor-mode))

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
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :bind
  (:map magit-status-mode-map
        ("@" . forge-dispatch))
  :hook
  (magit-mode . lkn/keychain-setup)
  (magit-post-refresh . magit-run-post-commit-hook)
  (magit-post-refresh . magit-run-post-stage-hook)
  (magit-post-refresh . magit-run-post-unstage-hook)
  :config
  ;; Prevent magit-auto-revert-mode from running
  (advice-add 'magit-auto-revert-mode :override #'ignore)

  ;; Or disable the underlying functionality
  (advice-add 'magit-auto-revert-buffers :override #'ignore)
  (setq transient-display-buffer-action '(display-buffer-below-selected)
        magit-display-buffer-function #'lkn/magit-fullscreen-same-windows
        magit-bury-buffer-function #'magit-restore-window-configuration)

  (defun lkn/magit-fullscreen-same-windows (buffer)
    "Display BUFFER, filling entire frame if BUFFER is a status buffer.
Otherwise, behave like `magit-display-buffer-traditional'."
    (if (eq (with-current-buffer buffer major-mode)
            'magit-status-mode)
        (display-buffer buffer '(magit--display-buffer-fullframe))
      (magit-display-buffer-same-window-except-diff-v1 buffer)))

  (defvar-local magit-stale-p nil)
  (defvar-local magit-refreshed-buffer nil)

  (defun magit-revertable-buffer-p (buffer)
    "Return non-nil if BUFFER should be auto-reverted."
    (and (buffer-live-p buffer)
         (or (buffer-file-name buffer)
             (buffer-local-value 'default-directory buffer))))

  (defun magit-revert-buffer (buffer)
    "Revert the current buffer and update the modeline."
    (with-current-buffer buffer
      (kill-local-variable 'magit-stale-p)
      (when (and buffer-file-name
                 (file-exists-p buffer-file-name)
                 (magit-auto-revert-repository-buffer-p buffer)
                 (not (buffer-modified-p buffer)))
        (revert-buffer t t t))
      (force-mode-line-update)))

  (defun magit-mark-stale-buffers-a (&rest _)
    "Revert all visible buffers and marked buried ones as stale."
    (let* ((visible-windows (window-list))
           (visible-buffers (mapcar #'window-buffer visible-windows)))
      (dolist (buffer (buffer-list))
        (when (magit-revertable-buffer-p buffer)
          (if (memq buffer visible-buffers)
              (progn
                (magit-revert-buffer buffer)
                (setq visible-buffers (delq buffer visible-buffers)))
            (with-current-buffer buffer
              (setq-local magit-stale-p t)))))))

  (defun magit-revert-buffer-maybe-h (&rest _window)
    "Revert the buffer if out of date."
    (when magit-stale-p
      (magit-revert-buffer (current-buffer))))

  (defun magit-set-window-state-h ()
    "Store the current buffer, point and window start before reverting."
    (setq-local magit-refreshed-buffer
                (list (current-buffer) (point) (window-start))))

  (defun magit-restore-window-state-h ()
    "Restore window state after reverting."
    (pcase-let ((`(,buf ,pt ,beg) magit-refreshed-buffer))
      (when (and buf (eq (current-buffer) buf))
        (goto-char pt)
        (set-window-start nil beg t)
        (kill-local-variable 'magit-refreshed-buffer))))

  (remove-hook 'post-command-hook #'magit-auto-revert-buffers)

  (add-hook 'buffer-list-update-hook #'magit-revert-buffer-maybe-h)
  (add-hook 'magit-post-refresh-hook #'magit-mark-stale-buffers-a)
  (add-hook 'window-buffer-change-functions #'magit-revert-buffer-maybe-h)
  (add-hook 'window-selection-change-functions #'magit-revert-buffer-maybe-h)

  (add-hook 'magit-pre-refresh-hook #'magit-set-window-state-h)
  (add-hook 'magit-post-refresh-hook #'magit-restore-window-state-h)
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
    (advice-add fn :around #'magit-git--memoize))

  ;; Custom helper functions to end up in jira-workflow one day

  (defun lkn/new-branch-pullreq ()
    "Create a new PR from the current branch, pushing if needed."
    (interactive)
    (require 'forge)
    (let ((current-branch (magit-get-current-branch)))
      (magit-fetch-all nil)

      (unless (magit-get-upstream-branch current-branch)
        (message "Setting upstream and pushing branch...")
        (magit-push-current-to-upstream nil))

      (message "Creating PR for branch %s onto %s" current-branch (magit-main-branch))
      (forge-create-pullreq (concat "origin/" current-branch) (concat "origin/" (magit-main-branch)))))

  (defun lkn/modify-pr-template (buf)
    "Adjust the PR buffer BUF to pre-fill some of the data"
    (interactive)
    (with-current-buffer buf
      (when (and (search-forward "CRM457" nil t) (eq major-mode 'forge-post-mode))
        (let* ((default-directory (magit-toplevel))
               (branch (magit-get-current-branch))
               (ticket-number (when (string-match "\\(CRM457-[0-9]+\\)" branch)
                                (match-string 1 branch)))
               (commits (magit-git-lines "rev-list" (concat (magit-get-current-branch) "..HEAD")))
               (commit-message (when (= (length commits) 1)
                                 (magit-git-string "log" "-1" "--format=%B" (car commits)))))

          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^---" nil t)
              (beginning-of-line)
              (kill-line 2)))

          (when (and ticket-number (search-forward "-XXX" nil t))
            (replace-match (substring ticket-number 6))
            (end-of-line)
            (delete-region (point) (point-max))

            (when (re-search-backward "^## Description of change" nil t)
              (forward-line 1)
              (when commit-message
                (insert "\n" (string-trim commit-message) "\n")
                (forward-line -1))
              (end-of-line))))))
    buf)

  (advice-add #'forge--prepare-post-buffer :filter-return #'lkn/modify-pr-template))

(use-package magit-todos
  :disabled t
  :after magit
  :config (magit-todos-mode 1))

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

(use-package pr-review
  :after (magit forge))

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
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'diff-hl-mode-hook 'diff-hl-show-hunk-mouse-mode)
  :custom
  (diff-hl-update-async t)
  :init
  (global-diff-hl-mode 1)
  :hook (diff-hl-mode . (lambda ()
                          (unless (display-graphic-p)
                            (diff-hl-margin-local-mode)))))

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
