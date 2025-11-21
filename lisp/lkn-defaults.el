;;; lkn-defaults.el -- Defaults for Emacs -*- lexical-binding: t -*-
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
           (require `,(intern (format "%s-theme" val)) nil t)
           (load-theme val t)))
  :group 'lkn)

(defcustom lkn-path '("~/.ghcup/bin"
                      "~/.local/share/mise/shims"
                      "~/.rbenv/shims"
                      "~/.local/share/nvim/mason/bin"
                      "~/.babashka/bbin/bin"
                      "~/.qlot/bin"
                      "~/.config/emacs/bin"
                      "~/.cargo/bin"
                      "~/bin"
                      "~/.local/bin"
                      "~/.config/yarn/global/node_modules/.bin"
                      "~/.dotnet/tools"
                      "~/.luarocks/bin"
                      "~/go/bin"
                      "/usr/local/bin"
                      "/usr/local/sbin"
                      "/usr/bin")
  "List of paths to setup to find needed executables."
  :type '(repeat string)
  :set (lambda (var val)
         (set-default var val)
         (setq exec-path (delete-dups (append val exec-path)))
         (setenv "PATH" (string-join exec-path ":")))
  :group 'lkn)

;;; Macros
;; Borrowed with love from progfolio
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

;; Any macros that have `!' after them can be considerd taken from
;; Doom Emacs.  At the time of writing, it's
;; <https://github.com/doomemacs/doomemacs/blob/ea616ebd5bcc98d342ab89bbe02f99dd8c0cd673/lisp/doom-lib.el>


(defmacro comment (&rest _body)
  "Take BODY but do nothing with it."
  nil)

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@BODY)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

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

;; Ensure that keychain's environment is setup in Emacs
;; Needed so magit can access GPG and SSH keys
;;;###autoload
(defun lkn/keychain-setup ()
  "Load keychain env after Emacs."
  (interactive)
  (unless (bound-and-true-p lkn/keychain-initialized-p)
    (let ((kill-buffer-query-functions nil))
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
               (kill-buffer "*keychain-output*"))
             (setq lkn/keychain-initialized-p t))))))))

;; Another Karthink gem, add repeat-mode support for a keymap
(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

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
    (if-let* ((buffer (find-file-noselect file)))
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
  (when-let* ((file (thread-first
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
      (prog-fill-reindent-defun)
    (fill-paragraph)))

(defun lkn/insert-zero-width-space ()
  "Insert a zero-width space character."
  (interactive)
  (insert "​"))

(defun lkn/split-window-below (&optional size window-to-split)
  "Call the original split-window but also focus after."
  (interactive `(,(when current-prefix-arg
                    (prefix-numeric-value current-prefix-arg))
                 ,(selected-window)))
  (split-window-below size window-to-split)
  (other-window 1))

(defun lkn/split-window-right (&optional size window-to-split)
  "Call the original split-window but also focus after."
  (interactive `(,(when current-prefix-arg
                    (prefix-numeric-value current-prefix-arg))
                 ,(selected-window)))
  (split-window-right size window-to-split)
  (other-window 1))

(defun lkn/sudo-kill-current-buffer ()
  "Like `kill-current-buffer' but also delete the file."
  (interactive)
  (let ((file (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and file (file-exists-p file)))
        (kill-current-buffer)
      (when (yes-or-no-p (concat "Are you sure you want to remove " file "? "))
        (delete-file file)
        (kill-buffer buffer)
        (message "File '%s' removed" file)))))

(defun lkn/rename-current-buffer-file ()
  "Rename the current buffer and visited file."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (unless (f-dir? (f-dirname new-name))
            (make-directory (f-dirname new-name)))
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun lkn/goto-line-feedback ()
  "Show the line numbers before executing `goto-line'."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (goto-line (read-number "Jump to line: ")))
    (display-line-numbers-mode -1)))

;;; Packages

(use-feature emacs
  :custom
  (trusted-content `("~/code/moj/"
                     ,user-emacs-directory))
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
  (ring-bell-function 'ignore)
  (use-short-answers t)
  (native-comp-async-report-warnings-errors 'silent)
  (select-enable-clipboard t)
  (compilation-scroll-output t)
  (confirm-nonexistent-file-or-buffer nil)
  (minibuffer-default-prompt-format " [%s]")
  (scroll-conservatively 101)
  (apropos-do-all t)
  (display-line-numbers 'relative)

  ;; Backups
  (backup-directory-alist
   '(("." . (lkn/cache-dir "backups"))))
  (make-backup-files nil)
  (backup-by-copying t)
  (create-lockfiles nil)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 3)
  (kept-old-versions 2)

  ;; Display
  (display-buffer-base-action
   '((display-buffer-reuse-window display-buffer-same-window)
     (reusable-frames . t)))
  (even-window-sizes nil)

  ;; Improve performance for files with long-lines
  (bidi-paragraph-direction 'left-to-right)
  :hook
  (before-save                  . executable-make-buffer-file-executable-if-script-p)
  (before-save                  . whitespace-cleanup)
  (after-init                   . pixel-scroll-precision-mode)
  (after-init                   . global-so-long-mode)
  (after-init                   . global-subword-mode)
  (after-init                   . delete-selection-mode)
  (after-init                   . lkn/keychain-setup)
  (after-init                   . winner-mode)
  (compilation-finish-functions . bury-compile-buffer-if-successful)
  (after-init                   . minibuffer-electric-default-mode)
  (after-init                   . global-completion-preview-mode)
  ;; Line numbers
  (prog-mode                    . display-line-numbers-mode)
  (help-mode                    . (lambda () (display-line-numbers-mode -1)))
  :bind
  (("C-x k"                    . kill-current-buffer)
   ("C-x C-k"                  . lkn/sudo-kill-current-buffer)
   ("C-x C-r"                  . lkn/rename-current-buffer-file)
   ("C-s"                      . isearch-forward-regexp)
   ("C-r"                      . isearch-backward-regexp)
   ("M-/"                      . hippie-expand)
   ("M-z"                      . zap-up-to-char)
   ("C-g"                      . lkn/keyboard-quit-dwim)
   ("M-q"                      . lkn/fill-region)
   ("C-c f p"                  . lkn/find-file-emacs)
   ("C-x p y"                  . lkn/yank-buffer-project-path)
   ([remap split-window-below] . lkn/split-window-below)
   ([remap split-window-right] . lkn/split-window-right))
  :init
  ;; Ensure the buffers are correctly encoded
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment   'utf-8)

  ;; Force this to be nil by default
  (setq-default indent-tabs-mode nil))

(use-feature server
  :when (display-graphic-p)
  :after exec-path-from-shell
  :config
  (unless
      (and (not (display-graphic-p))
           (server-running-p))
    (exec-path-from-shell-initialize)
    (atomic-chrome-start-server)
    (server-start)))

(use-feature savehist
  :after no-littering
  :custom
  (savehist-additional-variables '(kill-ring search-ring))
  (history-length 250)
  (kill-ring-max 25)
  :config
  (savehist-mode))

(use-feature saveplace
  :hook (elpaca-after-init . save-place-mode))

(use-feature recentf
  :init (recentf-mode)
  :bind
  ("C-c f r" . recentf)
  :after no-littering
  :custom
  (recentf-max-menu-items 200)
  (recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude LITTER-DIR))

(use-feature dired
  :commands (dired)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-dwim-target t)
  :hook
  (dired-mode . (lambda () (display-line-numbers-mode -1)))
  (dired-mode . dired-hide-details-mode)
  (dired-mode . hl-line-mode))

(use-feature repeat
  :hook (after-init . repeat-mode)
  :config
  (defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap))))

(use-feature treesit
  :custom
  (treesit-font-lock-level 5)
  :config
  ;; Ruby setup
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-ts-mode))
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode)))

(use-feature autorevert
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-avoid-polling t)
  (revert-without-query (list "."))
  :hook
  (after-save . lkn/auto-revert-buffers)
  :config
  (defun lkn/auto-revert-buffer (&optional _window)
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
  (add-hook 'window-buffer-change-functions #'lkn/auto-revert-buffer)
  (add-function
   :after after-focus-change-function
   #'lkn/auto-revert-buffers))

(use-feature uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator  " • ")
  (uniquify-after-kill-buffer-p  t)
  (uniquify-ignore-buffers-re  "^\\*" ))

(use-feature xref
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function #'xref-show-definitions-buffer)
  (xref-file-name-display 'project-relative))

(use-feature flymake
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-supress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  :bind
  (:map flymake-mode-map
        ([remap next-error] . flymake-goto-next-error)
        ([remap previous-error] . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-feature outline
  :custom
  (outline-minor-mode-highlight t)
  (outline-minor-mode-cycle t)
  :init
  (defcustom outline-minor-mode-characters " ▼ "
    "Characters to use in-place of the default ellipsis for outline-minor-mode."
    :type 'string)

  (defun lkn/outline-mode-ellipsis ()
    "Apply the ellipsis to outline mode locally to a buffer."
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c) (+ face-offset c)) outline-minor-mode-characters))))
      (set-display-table-slot display-table 'selective-display value)
      (setq buffer-display-table display-table)))

  (add-hook 'outline-minor-mode-hook #'lkn/outline-mode-ellipsis))

(use-feature elec-pair
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate #'lkn/electric-pair-inhibit)
  :init
  (defun lkn/electric-pair-inhibit (char)
    "Adjust how we pair up CHAR depending on context."
    (or
     ;; I find it more often preferable not to pair when the
     ;; same char is next.
     (eq char (char-after))
     ;; Don't pair up when we insert the second of "" or of ((.
     (and (eq char (char-before))
          (eq char (char-before (1- (point)))))
     ;; I also find it often preferable not to pair next to a word.
     (eq (char-syntax (following-char)) ?w)
     ;; Don't pair at the end of a word, unless parens.
     (and
      (eq (char-syntax (char-before (1- (point)))) ?w)
      (eq (preceding-char) char)
      (not (eq (char-syntax (preceding-char)) ?\())))))

(use-feature smerge-mode
  :config
  (repeatize 'smerge-basic-map))

(use-feature edebug
  :after eros
  :config
  ;; Thanks to https://xenodium.com/inline-previous-result-and-why-you-should-edebug/
  (require 'eros)

  (defun adviced:edebug-previous-result (_ &rest r)
    "Adviced `edebug-previous-result'."
    (eros--make-result-overlay edebug-previous-result
      :where (point)
      :duration eros-eval-result-duration))

  (advice-add #'edebug-previous-result
              :around
              #'adviced:edebug-previous-result))

(provide 'lkn-defaults)
;;; lkn-defaults.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
