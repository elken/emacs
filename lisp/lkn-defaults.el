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
;;; Just a bunch of default settings.  Any built-in modes etc.
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
;; Any macros that have `!' after them can be considerd taken from
;; Doom Emacs.  At the time of writing, it's
;; <https://github.com/doomemacs/doomemacs/blob/ea616ebd5bcc98d342ab89bbe02f99dd8c0cd673/lisp/doom-lib.el>

(defmacro comment (&rest body)
  "Take BODY but do nothing with it."
  nil)

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@BODY)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order, the following REST args:

  1. The mode(s) or hook(s) to add to.  This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.

Takes the same REST arguments as `add-hook!'.

If N and M = 1, there's no benefit to using this macro over `remove-hook'.

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest VAR-VALS [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (doom--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'.  WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.

This has the same signature as `defadvice!' an exists as an easy undefiner when
testing advice (when combined with `rotate-text').

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

;;; Functions

(defun lkn/swap-face (face &optional frame inherit)
  "Given a FACE, swap the background and foreground.
Same as `invert-face' but returns the new face rather than setting
everything.

Optionally take the same FRAME and INHERIT arguments that
`face-attribute' expects."
  (let ((fg (face-foreground face frame inherit))
	(bg (face-background face frame inherit)))
    `(:foreground ,bg :background ,fg)))

(defun lkn/combine-faces (&rest faces)
  "Given a list of FACES, apply them together and return the spec."
  (let ((attrs '(:foreground :background :weight :slant :underline)))
    `((t ,@(cl-loop for attr in attrs
                    for val = (cl-some (lambda (face)
					 (cond
					  ((facep face)
                                           (let ((v (face-attribute face attr)))
                                             (unless (eq v 'unspecified) v)))
					  ((listp face)
                                           (plist-get face attr))))
                                       faces)
                    when val collect attr and collect val)))))

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
  "Apply the list of modes to not display line numbers for."
  (dolist (mode lkn/global-no-numbers-modes)
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Ensure that keychain's environment is setup in Emacs
;; Needed so magit can access GPG and SSH keys
;;;###autoload
(defun lkn/keychain-setup ()
  "Load keychain env after Emacs."
  (interactive)
  (when (executable-find "keychain")
    (make-process
     :name "keychain-setup"
     :command '("keychain" "-q" "--noask" "--agents" "ssh,gpg" "--eval")
     :buffer "*keychain-output*"
     :sentinel
     (lambda (process _event)
       (when (eq 'exit (process-status process))
         (with-current-buffer (process-buffer process)
           (let ((output (buffer-string)))
             (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
               (when (string-match (format "%s[=\s]\\([^\s;\n]*\\)" var) output)
                 (setenv var (match-string 1 output))))))
	 (when (buffer-live-p (get-buffer "*keychain-output*"))
           (kill-buffer "*keychain-output*")))))))

;; Only show the compilation buffer if there are errors. Otherwise,
;; it's useless
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation BUFFER if succeeded without warnings by checking STRING contents."
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
  "Yank the contents of the current buffer."
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
  "WIP: ARGUMENT."
  (interactive "P")
  (if argument
      (call-interactively 'fill-paragraph)
    (prog-fill-reindent-defun)))

;;; Packages

(use-package emacs
  :ensure nil
  :custom
  (source-directory (expand-file-name "~/code/emacs"))
  (next-error-recenter '(4))
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
  (confirm-nonexistent-file-or-buffer nil)
  ;; Backups
  (backup-directory-alist
   '(("." . (lkn/cache-dir "backups"))))
  (backup-by-copying t)
  (create-lockfiles nil)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 3)
  (kept-old-versions 2)
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
   ("C-x p y" . lkn/yank-buffer-project-path))
  :init
  ;; Ensure the buffers are correctly encoded
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8))

(use-package savehist
  :ensure nil
  :after no-littering
  :custom
  (savehist-additional-variables '(kill-ring search-ring))
  (history-length 250)
  (kill-ring-max 25)
  :config
  (savehist-mode))

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :after no-littering
  :custom
  (recentf-max-menu-items 200)
  (recentf-max-saved-items 200)
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

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-avoid-polling t)
  (revert-without-query (list "."))
  :hook
  (focus-in . lkn/auto-revert-buffers)
  (after-save . lkn/auto-revert-buffers)
  :config
  (defun lkn/auto-revert-buffer (&optional window)
    "Auto revert current buffer, if necessary."
    (unless (or (and (bound-and-true-p auto-revert-mode)
		     auto-revert-mode)
		(active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun lkn/auto-revert-buffers ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (delete-dups
		  (cl-loop for frame in (visible-frame-list)
			   if (window-list frame)
			   nconc (mapcar #'window-buffer it))))
      (with-current-buffer buf
        (lkn/auto-revert-buffer))))

  (add-hook 'window-selection-change-functions #'lkn/auto-revert-buffer)
  (add-hook 'window-buffer-change-functions #'lkn/auto-revert-buffer))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator  " • ")
  (uniquify-after-kill-buffer-p  t)
  (uniquify-ignore-buffers-re  "^\\*" ))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

(use-package flymake
  :ensure nil
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  :bind
  (:map flymake-mode-map
	([remap next-error] . flymake-goto-next-error)
	([remap previous-error] . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(provide 'lkn-defaults)
;;; lkn-defaults.el ends here
