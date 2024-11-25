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


(use-package magit)

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
  (setq transient-levels-file  (expand-file-name "transient/levels" no-littering-var-directory)
        transient-values-file  (expand-file-name "transient/values" no-littering-var-directory)
        transient-history-file (expand-file-name "transient/history" no-littering-var-directory)
        transient-default-level 5))

(provide 'lkn-vcs)
;;; lkn-vcs.el ends here
