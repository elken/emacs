;;; lkn-defaults.el -- Defaults for Emacs -*- lexical-binding: t -*-
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
;;; Just a bunch of default settings. Any built-in modes etc.
;;; Code:

(defcustom lkn-default-font "MonaspiceNe NFM"
  "The font used in the default face.
This should be a nice, readable mono-space font with ideally
support for nerd icons."
  :type 'string
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (set-default var val)
	 (set-face-attribute 'default nil :font val)
	 (set-face-attribute 'fixed-pitch nil :font val))
  :group 'lkn)

(defcustom lkn-default-font-height 150
  "The default font height to use."
  :type 'number
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (set-default var val)
	 (set-face-attribute 'default nil :height val)
	 (set-face-attribute 'fixed-pitch nil :height val))
  :group 'lkn)

(defcustom lkn-variable-pitch-font "Merriweather"
  "The font used in the variable and mixed-pitch modes.
This should just be a nice, readable font to represent prose well."
  :type 'string
  :initialize 'custom-initialize-set
  :set (lambda (var val)
	 (set-default var val)
	 (set-face-attribute 'variable-pitch nil :font val))
  :group 'lkn)

(defcustom lkn-theme 'doom-nord
  "The default flavour-of-the-month theme.
Usually defaults to Nord or my Carbon theme."
  :type 'symbol
  :set (lambda (var val)
	 (set-default var val)
	 (with-eval-after-load 'doom-themes
	   (require `,(intern (format "%s-theme" val)))
	   (load-theme val t)))
  :group 'lkn)

;;; Macros

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

;;; Functions

;; Borrowed with love from prot
(defun lkn/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; Setup modes to disable line numbers
(defvar lkn/global-no-numbers-modes
  '(org-mode-hook
    term-mode-hook
    shell-mode-hook
    treemacs-mode-hook
    pdf-view-mode
    eshell-mode-hook)
  "List of modes to not display line numbers on.")

;; Globally disable hooks
(defun lkn/disable-hooks-setup ()
  (dolist (mode lkn/global-no-numbers-modes)
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Ensure that keychain's environment is setup in Emacs
;; Needed so magit can access GPG and SSH keys
(defun lkn/keychain-setup ()
  "Load keychain env after emacs"
  (interactive)
  (when (executable-find "keychain")
    (let* ((ssh (shell-command-to-string "keychain -q --noask --agents ssh --eval"))
           (gpg (shell-command-to-string "keychain -q --noask --agents gpg --eval")))
      (list (and ssh
                 (string-match "SSH_AUTH_SOCK[=\s]\\([^\s;\n]*\\)" ssh)
                 (setenv       "SSH_AUTH_SOCK" (match-string 1 ssh)))
            (and ssh
                 (string-match "SSH_AGENT_PID[=\s]\\([0-9]*\\)?" ssh)
                 (setenv       "SSH_AGENT_PID" (match-string 1 ssh)))
            (and gpg
                 (string-match "GPG_AGENT_INFO[=\s]\\([^\s;\n]*\\)" gpg)
                 (setenv       "GPG_AGENT_INFO" (match-string 1 gpg)))))))

;; Only show the compilation buffer if there are errors. Otherwise,
;; it's useless
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and (eq major-mode 'comint-mode)
             (string-match "finished" string)
             (not
	      (with-current-buffer buffer
                (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
		      (let ((window (get-buffer-window buf)))
                        (when (and (window-live-p window)
                                   (eq buf (window-buffer window)))
                          (delete-window window))))
                    buffer)))

;; Borrowed with love from Doom
(defun lkn/sudo-save-buffer ()
  "Save the current buffer as root."
  (interactive)
  (let ((file (format "/sudo::%s" buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
	      (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
	      (kill-buffer buffer))
            (with-current-buffer origin
	      (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

(defun lkn/yank-buffer ()
  "Yank the contents of the current buffer"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun lkn/yank-buffer-path (&optional root)
  "Copy path to file, optionally relative to ROOT."
  (interactive)
  (when-let ((file (thread-first
                     (buffer-base-buffer)
                     buffer-file-name
                     abbreviate-file-name))
             (path (if root (file-relative-name file root) file)))
    (kill-new path)
    (message "Copied %s" path)))

(defun lkn/yank-buffer-project-path ()
  "Copy path to file relative to project root."
  (interactive)
  (lkn/yank-buffer-path (project-root (project-current))))

;; Search in my Emacs config directory
(defun lkn/find-file-emacs ()
  "Find file relative to Emacs' start-up directory."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

(defun lkn/fill-region (&optional argument)
  (interactive "P")
  (if argument
      (call-interactively 'fill-paragraph)
    (prog-fill-reindent-defun)))

;;; Packages

(use-package emacs
  :ensure nil
  :custom
  (scroll-preserve-screen-position t)
  (confirm-kill-emacs #'y-or-n-p)
  (frame-title-format '(:eval
			(format "[%%b%s] - %s"
                                (if (buffer-modified-p) " •" "")
                                system-name)))
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (vc-follow-symlinks nil)
  (create-lockfiles nil)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)
  (use-short-answers t)
  (native-comp-async-report-warnings-errors 'silent)
  (select-enable-clipboard t)
  (compilation-scroll-output t)
  :hook
  (text-mode . display-line-numbers-mode)
  (prog-mode . display-line-numbers-mode)
  (before-save . executable-make-buffer-file-executable-if-script-p)
  (after-init . lkn/disable-hooks-setup)
  (after-init . pixel-scroll-precision-mode)
  (after-init . global-so-long-mode)
  (after-init . electric-pair-mode)
  (after-init . global-subword-mode)
  (after-init . delete-selection-mode)
  (after-init . lkn/keychain-setup)
  (compilation-finish-functions . bury-compile-buffer-if-successful)
  :bind
  (("C-x k" . kill-current-buffer)
   ("C-s" . save-buffer)
   ("C-g" . lkn/keyboard-quit-dwim)
   ("M-q" . lkn/fill-region)
   ("C-c f p" . lkn/find-file-emacs)
   ("C-x p y" . lkn/yank-buffer-project-path)))

(use-package savehist
  :ensure nil
  :after no-littering
  :custom
  (savehist-additional-variables '(kill-ring search-ring))
  :config
  (savehist-mode))

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :after no-littering
  :config
  (add-to-list 'recentf-exclude LITTER-DIR))

(use-package dired
  :ensure nil
  :commands (dired)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 5))

(provide 'lkn-defaults)
;;; lkn-defaults.el ends here
