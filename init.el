;;; init.el -- All roads lead here -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2025 Ellis Kenyő <emacs@lkn.mozmail.com>

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
;;; Nothing here but us chickens... well, this chicken
;;; Code:

(setopt user-full-name "Ellis Kenyő"
        user-mail-address "emacs@lkn.mozmail.com")

(setopt custom-file (make-temp-file "el-custom-"))

(defvar after-load-theme-hook nil
  "Hook run after the color theme is loaded with `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(dolist (path '("modules" "lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(defgroup lkn ()
  "Customization group for any settings specific to me."
  :group 'tools)

;; Package management setup
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" LITTER-DIR))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setopt dired-listing-switches "-ahl -v --group-directories-first --color=auto")

(use-package benchmark-init
  :demand t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package diminish
  :demand t
  :diminish (subword-mode eldoc-mode))

(use-package no-littering
  :demand t
  :init
  (setopt no-littering-etc-directory (expand-file-name "config/" LITTER-DIR)
          no-littering-var-directory (expand-file-name "share/" LITTER-DIR)
          auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" no-littering-var-directory) t)))

  (unless (file-exists-p (expand-file-name "cache" no-littering-var-directory))
    (mkdir (expand-file-name "cache" no-littering-var-directory)))

;;;###autoload
  (defun lkn/config-dir (file)
    "Given FILE, return a valid path relative to `no-littering-etc-directory'."
    (expand-file-name file no-littering-etc-directory))

;;;###autoload
  (defun lkn/cache-dir (file)
    "Given FILE, return a valid path relative to `no-littering-var-directory/cache'."
    (expand-file-name file (expand-file-name "cache" no-littering-var-directory))))

(use-package gcmh
  :custom
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-idle-delay 'auto)
  :diminish gcmh-mode
  :hook (emacs-startup . gcmh-mode)
  :config
  (add-function
   :after after-focus-change-function
   #'garbage-collect))

(use-package exec-path-from-shell
  :demand t
  :custom
  (exec-path-from-shell-arguments '("-l")))

(use-package async
  :custom
  (message-send-mail-function 'async-smtpmail-send-it)
  :hook (dired-mode . dired-async-mode))

(use-package server
  :ensure nil
  :after exec-path-from-shell
  :defer 1
  :config
  (unless (server-running-p)
    (exec-path-from-shell-initialize)
    (server-start)))

(require 'lkn-defaults)
(require 'lkn-workspaces)
(require 'lkn-ui)
(require 'lkn-edit)
(require 'lkn-meow)
(require 'lkn-vcs)
(require 'lkn-org)
(require 'lkn-project)
(require 'lkn-terms)
(require 'lkn-lsp)
(require 'lkn-langs)
(require 'lkn-ruby)
(require 'lkn-lisp)
(require 'lkn-docs)
(require 'lkn-completion)
(when IS-MAC
  (require 'lkn-macos))
(require 'lkn-llm)

(use-package request)
(use-package jira-workflow
  :ensure (:host github :repo "elken/jira-workflow"))

(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (load (expand-file-name "init-local.el" user-emacs-directory)))

(provide 'init)
;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
