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

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setopt dired-listing-switches "-ahl -v --group-directories-first")
(setopt scroll-preserve-screen-position t)
(setopt confirm-kill-emacs #'yes-or-no-p)

(setopt frame-title-format '(:eval
                             (format "[%%b%s] - %s"
                                     (if (buffer-modified-p) " •" "")
                                     system-name)))

(setopt create-lockfiles nil
	make-backup-files nil)

(setopt ring-bell-function 'ignore)

;; Line numbers good
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setopt display-line-numbers-type 'relative)

;; Line numbers bad
(defvar lkn/global-no-numbers-modes
  '(org-mode-hook
    term-mode-hook
    shell-mode-hook
    treemacs-mode-hook
    pdf-view-mode
    eshell-mode-hook)
  "List of modes to not display line numbers on.")

(defun lkn/disable-hooks-setup ()
  (dolist (mode lkn/global-no-numbers-modes)
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(add-hook 'after-init-hook #'lkn/disable-hooks-setup)

(setopt use-short-answers t)

(when (native-comp-available-p)
  (setopt native-comp-async-report-warnings-errors 'silent))

(add-hook 'before-save-hook #'executable-make-buffer-file-executable-if-script-p)

(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

(global-so-long-mode)

(savehist-mode)

(electric-pair-mode)

(global-subword-mode)

(blink-cursor-mode -1)

(defun lkn/keychain-setup ()
  "Load keychain env after emacs"
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

(add-hook 'after-init-hook #'lkn/keychain-setup)

(keymap-global-set "C-x k" #'kill-current-buffer)
(keymap-global-set "C-s" #'save-buffer)
(keymap-global-set "<escape>" #'keyboard-escape-quit)

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

(add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful)

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

(defun lkn/find-file-emacs ()
  "Find file relative to Emacs' start-up directory."
  (interactive)
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

(defmacro cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (&rest _) (interactive) ,@body))

(defun lkn/meow-kill-dwim ()
  "DWIM delete that preserves the kill ring."
  (interactive)
  (if (region-active-p)
      (progn
	(kill-region (region-beginning) (region-end))
	(meow-cancel-selection))
    (if smartparens-strict-mode
	(when-let ((char (char-after)))
	  (sp-delete-char 1)
	  (kill-new (char-to-string char)))
      (kill-forward-chars 1))))

(defun lkn/meow-delete-dwim ()
  "DWIM delete that doesn't use the kill ring."
  (interactive)
  (if (region-active-p)
      (progn
	(delete-region (region-beginning) (region-end))
	(meow-cancel-selection))
    (if smartparens-strict-mode
	(sp-delete-char 1)
      (delete-char 1))))

(defun lkn/meow-append-after ()
  "Behave more like Vim/Helix's `a'."
  (interactive)
  (if (region-active-p)
      (progn
	(goto-char (region-end))
	(meow-cancel-selection)))
  (forward-char)
  (meow-insert))

(defvar-keymap lkn/meow-g-command-map
  :doc "Keymap for g-prefixed commands"
  "c" #'comment-dwim
  "g" #'beginning-of-buffer)

;; Meow setup function - moved outside use-package for clarity
(defun meow-setup ()
  (require 'meow)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow--kbd-delete-char "<deletechar>")
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("u" . universal-argument))
  
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("/" . consult-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("%" . lkn/goto-matching-paren)
   '("=" . indent-region)
   '("a" . lkn/meow-append-after)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . lkn/meow-kill-dwim)
   '("D" . lkn/meow-delete-dwim)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   (cons "g" lkn/meow-g-command-map)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-grab)
   '("M" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . undo-redo)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("C-u" . meow-page-up)
   '("C-d" . meow-page-down)
   '("<escape>" . meow-cancel-selection)))

(set-face-attribute 'default nil :font "MonaspiceNe NFM" :height 150)
(set-face-attribute 'variable-pitch nil :font "Montserrat")
(savehist-mode)

(defun lkn/goto-matching-paren (&optional arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-sexp 1))
        ((looking-back "\\s\)" 1) (backward-sexp 1))
        ;; Now, try the other direction
        ((looking-at "\\s\)") (forward-char) (backward-sexp 1))
        ((looking-back "\\s\(" 1) (backward-char) (forward-sexp 1))))

(use-package no-littering
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

(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :after no-littering
  :config
  (add-to-list 'recentf-exclude LITTER-DIR))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :hook (LaTeX-mode . mixed-pitch-mode)
  :hook (markdown-mode . mixed-pitch-mode)
  :hook (gfm-mode . mixed-pitch-mode)
  :hook (Info-mode . mixed-pitch-mode)
  :hook (rst-mode . mixed-pitch-mode)
  :hook (adoc-mode . mixed-pitch-mode))

(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :init
  (setopt apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(use-package apheleia
  :init
  (apheleia-global-mode)
  :config
  (cl-defun save-buffer-maybe-format (orig-fn &optional arg)
    "Allow universal argument to disable formatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall orig-fn)))
  (advice-add 'save-buffer :around #'save-buffer-maybe-format))

(use-package vertico
  :after meow
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t)
  (completion-in-region-function #'consult-completion-in-region)
  :custom-face
  (vertico-quick1 ((t (:foreground unspecified :background unspecified :inverse t :inherit (warning bold)))))
  (vertico-quick2 ((t (:foreground unspecified :background unspecified :inverse t :inherit (highlight bold)))))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (vertico-mode . vertico-mouse-mode)
  :init
  (meow-leader-define-key
   '("'" . vertico-repeat))
  (vertico-mode)
  :bind
  (:map vertico-map
        ([backspace] . vertico-directory-delete-char)
        ("M-q" . vertico-quick-insert)
        ("C-q" . vertico-quick-exit)))

(use-package embark
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act)
   ([remap describe-bindings]. embark-bindings)
   (:map minibuffer-local-map
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package embark-vc
;;   :after (embark magit)
;;   :vc (:url "https://github.com/elken/embark-vc"))

(use-package magit)
(use-package forge
  :commands (forge-create-pullreq forge-create-issue)
  :preface
  (setq forge-database-file (lkn/cache-dir "forge/forge-database.sqlite")
        forge-add-default-bindings nil
        forge-owned-accounts '(("elken")))
  :bind
  ((:map forge-topic-list-mode-map
         ("q" . kill-current-buffer))
   (:map magit-mode-map
         ([remap magit-browse-thing] . forge-browse-dwim))
   (:map magit-remote-section-map
         ([remap magit-browse-thing] . forge-browse-remote))
   (:map magit-branch-section-map
         ([remap magit-browse-thing] . forge-browse-branch))))

(use-package code-review
  :vc (:url "https://github.com/doomelpa/code-review")
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
  :ensure nil
  :pin "melpa"
  :bind
  (:map transient-map
        ([escape] . transient-quit-one))
  :init
  (setq transient-levels-file  (expand-file-name "transient/levels" no-littering-var-directory)
        transient-values-file  (expand-file-name "transient/values" no-littering-var-directory)
        transient-history-file (expand-file-name "transient/history" no-littering-var-directory)
        transient-default-level 5))

(use-package hide-mode-line)

(use-package vterm
  :commands (vterm vterm-other-window lkn/vterm-toggle)
  :hook (vterm-mode . hide-mode-line-mode)
  :custom
  (vterm-shell "/bin/zsh")
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  :config
  (with-eval-after-load 'meow
    (meow-leader-define-key
     '("v" . lkn/vterm-toggle)))
  (define-key vterm-mode-map (kbd "<C-backspace>") (cmd! (vterm-send-key (kbd "C-w"))))
  (add-hook 'vterm-exit-functions (cmd! (unless (one-window-p) (kill-buffer-and-window))))

  ;; TODO Tidy these up, `add-to-list' maybe?
  (setf (alist-get "woman" vterm-eval-cmds nil nil #'equal)
        '((lambda (topic)
            (woman topic))))
  (setf (alist-get "magit-status" vterm-eval-cmds nil nil #'equal)
        '((lambda (path)
            (magit-status path))))
  (setf (alist-get "dired" vterm-eval-cmds nil nil #'equal)
        '((lambda (dir)
            (dired dir))))

  ;; Always open vterm buffers on the bottom & default height
  (setf (alist-get "\\*vterm\\*" display-buffer-alist nil nil #'equal)
        '((display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.2)))

  ;; I only ever want 1 per project, if I /really/ need another I can
  ;; manually invoke `vterm'
  ;;;###autoload
  (defun lkn/vterm-toggle ()
    "Toggle a vterm buffer.
Based on perspective, if a buffer already exists switch to it. Else
create it."
    (interactive)
    (let ((buffer
           (seq-find (lambda (buffer)
                       (and (buffer-live-p buffer)
                            (string= "*vterm*" (buffer-name buffer))))
                     (persp-current-buffers* t))))
      (when-let ((root (project-current)))
        (setq-local default-directory (project-root root)))
      (if buffer
          (if-let (win (get-buffer-window buffer))
              (delete-window win)
            (pop-to-buffer buffer))
        (vterm-other-window)))))

(use-package meow-vterm
  :vc (:url "https://github.com/accelbread/meow-vterm")
  :after (meow vterm)
  :hook (meow-mode . meow-vterm-enable))

(use-package perspective
  :init (persp-mode)
  :hook (kill-emacs . persp-state-save)
  :custom
  (persp-suppress-no-prefix-key-warning t)
  (persp-show-modestring nil)
  (persp-state-default-file (lkn/cache-dir "perspectives"))
  :config
  ;; Ensure the state always saves on exit
  (add-hook 'kill-emacs-hook #'persp-state-save)

  ;; Adjust the bindings to work with meow
  (with-eval-after-load 'which-key
    (which-key-add-keymap-based-replacements perspective-map
      "1" "switch to 1"
      "2" "switch to 2"
      "3" "switch to 3"
      "4" "switch to 4"
      "5" "switch to 5"
      "6" "switch to 6"
      "7" "switch to 7"
      "8" "switch to 8"
      "9" "switch to 9"
      "0" "switch to 10"))
  
  (with-eval-after-load 'meow
    (defvar-keymap perspective-map
      :prefix 'perspective-map
      "n" #'perspective-next
      "p" #'perspective-prev
      "s" #'perspective-switch
      "S" #'perspective-save-state
      "L" #'perspective-load-state
      "a" #'perspective-add-buffer
      "r" #'perspective-rename
      "x" #'perspective-kill
      "c" #'perspective-create
      "b" #'persp-switch-to-buffer
      "`" #'persp-switch-by-number
      "k" #'persp-remove-buffer
      "1" (lambda () (interactive) (persp-switch-by-number 1))
      "2" (lambda () (interactive) (persp-switch-by-number 2))
      "3" (lambda () (interactive) (persp-switch-by-number 3))
      "4" (lambda () (interactive) (persp-switch-by-number 4))
      "5" (lambda () (interactive) (persp-switch-by-number 5))
      "6" (lambda () (interactive) (persp-switch-by-number 6))
      "7" (lambda () (interactive) (persp-switch-by-number 7))
      "8" (lambda () (interactive) (persp-switch-by-number 8))
      "9" (lambda () (interactive) (persp-switch-by-number 9))
      "0" (lambda () (interactive) (persp-switch-by-number 10)))

    (meow-leader-define-key
     '("," . persp-switch-to-buffer*)
     (cons "TAB" perspective-map)))
  
  ;; Inspired by <https://github.com/bbatsov/persp-projectile>
  (defadvice project-switch-project (around project-persp-switch-project (project) activate)
    "Switch to perspective for project."
    (interactive (list (project-prompt-project-dir)))
    (let* ((name (project-name (project-current nil project)))
           (persp (gethash name (perspectives-hash)))
           (command (if (symbolp project-switch-commands)
                        project-switch-commands
                      (project--switch-project-command)))
           (project-current-directory-override project))
      (persp-switch name)
      (unless (equal persp (persp-curr))
        (call-interactively command))))

  (defadvice persp-init-frame (after project-persp-init-frame activate)
    "Rename initial perspective to the project name when a new frame
is created in a known project."
    (with-selected-frame frame
      (when (project-current)
        (persp-rename (project-name (project-current)))))))

(use-package doom-themes
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :init
  (load-theme 'doom-nord t)
  (custom-theme-set-faces
   'doom-nord
   `(corfu-default ((t (:font "MonaspiceNe NFM" :background ,(doom-color 'bg-alt) :foreground ,(doom-color 'fg)))))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode)
  :config
  (setq marginalia-censor-variables nil)

  (defun marginalia--annotate-local-file (cand)
    "Improved local file annotations for CAND, coloured based on recency."
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (declare-function doom-blend "doom-themes")
  (defun +marginalia--time-colorful (time)
    "Improved local file annotations, coloured based on recency based on TIME."
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    "Improved local file annotations, coloured based on SIZE."
    (let* ((size-index (/ (log (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package consult
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :bind
  (([remap apropos] . consult-apropos)
   ([remap imenu]   . consult-imenu)
   ([remap load-theme] . consult-theme)
   ([remap man]        . consult-man)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame))
  :config
  (meow-leader-define-key
   '("/" . consult-ripgrep))
  (consult-customize
   consult-ripgrep consult-grep consult-git-grep consult-recent-file
   :preview-key "C-SPC")
  (consult-customize consult-theme
                     :preview-key (list "C-SPC" :debounce 0.5 'any)))

(use-package consult-dir
  :bind
  (([remap list-directory] . consult-dir)
   :map vertico-map
   ("C-x C-j" . consult-dir-jump-file))
  :config
  (meow-leader-define-key
   '("-" . consult-dir--source-tramp-local))
  
  (defun lkn/consult-dir-podman-hosts ()
    "Get a list of hosts from podman."
    (cl-loop
     for line in (cdr
		  (ignore-errors
		    (apply #'process-lines "podman"
			   (list "ps"))))
     for cand = (split-string line "[[:space:]]+" t)
     collect (let ((user (unless (string-empty-p (car cand))
			   (concat (car cand) "@")))
		   (hostname (car (last cand))))
	       (format "/podman:%s:/" hostname))))

  (defvar lkn/consult-dir-podman-source
    `(:name    "Podman"
      :narrow  ?d
      :category file
      :face     consult-file
      :history  file-name-history
      :items    ,#'lkn/consult-dir-podman-hosts))

  (add-to-list 'consult-dir-sources 'lkn/consult-dir-podman-source t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package meow
  :demand t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless partial-completion))
  (completion-category-overrides '((file (styles . (partial-completion)))
				   (eglot (styles orderless))
				   (eglot-capf (styles orderless))))
  (orderless-component-separator "[ &]")
  (completion-category-defaults nil))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setopt which-key-idle-delay 0.3
          which-key-separator " → "
          which-key-sort-order #'which-key-key-order-alpha
          which-key-sort-uppercase-first nil
          which-key-max-description-length 30
          which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 6
          which-key-side-window-slot -10
          which-key-allow-multiple-replacements t
          which-key-ellipsis "…")
  (let ((replacements (list '("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)")
                            '("\\`g s" . "\\`evilem--?motion-\\(.*\\)"))))
    ;; Iterate over each replacement
    (dolist (replacement replacements which-key-replacement-alist)
      (let ((formatted-replacement (cons replacement '(" \\1"))))
        ;; Only add replacement if it's not already present
        (unless (member formatted-replacement which-key-replacement-alist)
          (push formatted-replacement which-key-replacement-alist)))))

  (which-key-setup-side-window-bottom))

(use-package corfu
  :demand t
  :bind
  (:map corfu-map
	("C-SPC" . corfu-insert-separator)
	("C-n"   . corfu-next)
	("C-p"   . corfu-previous)
	("C-q"   . corfu-quick-insert))
  :hook
  (corfu-history-mode . corfu-mode)
  (corfu-echo-mode . corfu-mode)
  (corfu-popupinfo-mode . corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (corfu-min-width 50)
  :custom-face
  (corfu-quick1 ((t (:foreground unspecified :background unspecified :inherit vertico-quick1))))
  (corfu-quick2 ((t (:foreground unspecified :background unspecified :inherit vertico-quick2))))
  :init (global-corfu-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (setq corfu-margin-formatters '(nerd-icons-corfu-formatter)))

(use-package nerd-icons
  :config
  (when (and (not (member "Symbols Nerd Font Mono" (font-family-list)))
	     (display-graphic-p))
    (nerd-icons-install-fonts t))
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Fonts Mono") nil 'prepend))

(use-package cape
  :after corfu
  :bind
  (([remap dabbrev-expand] . cape-dabbrev))
  :init
  (defun lkn/corfu--latex-set-capf ()
    (add-to-list 'completion-at-point-functions #'cape-tex))
  (add-hook 'latex-mode-hook #'lkn/corfu--latex-set-capf)

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(use-package package-capf
  :vc (:url "https://github.com/elken/package-capf")
  :after cape
  :hook (emacs-lisp-mode . lkn/corfu--set-emacs-lisp-capfs)
  :init
  (defun lkn/corfu--set-emacs-lisp-capfs ()
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'package-capf)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; Replace exec-path-from-shell with direct path setting
(when (memq window-system '(mac ns x))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/opt/homebrew/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin" "/opt/homebrew/bin"))))

(use-package rbenv
  :defer t
  :hook (ruby-mode . global-rbenv-mode))

(use-package tempel
  :bind
  (("C-c s" . tempel-complete)
   (:map tempel-map
	 ([tab] . tempel-next)
	 ([backtab] . tempel-previous)))
  :config
  (defun lkn-snippets-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
	  (cons 'l template)
	(message "Template %s not found" (cadr elt))
	nil)))

  (add-to-list 'tempel-user-elements #'lkn-snippets-include))

(use-package popper
  :after perspective
  :init (popper-mode)
  :hook (popper-mode . popper-echo-mode)
  :bind (("M-`"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-mode-line
   '(:eval
     (propertize " 󰍡" 'face 'mode-line-emphasis)))
  (popper-group-function #'popper-group-by-perspective)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     helpful-mode
     Info-mode
     vterm-mode
     org-journal-mode
     "\\*Org Select\\*"
     compilation-mode)))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-use-scaling t)
  (pdf-view-use-imageamgick nil))

(use-package saveplace-pdf-view
  :after pdf-view)

;; Eglot configuration
(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  :hook
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(ruby-mode . ("ruby-lsp")))
  (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("ruby-lsp")))
  (setq-default eglot-workspace-configuration
		'((:ruby-lsp . (:formatter (:enable t))))))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package elixir-ts-mode
  :hook ((elixir-mode elixir-ts-mode) . eglot-ensure)
  :after treesit-auto)

(use-package smartparens
  :defer t
  :hook ((prog-mode text-mode org-mode markdown-mode) . smartparens-strict-mode)
  :config
  (require 'smartparens-config))

(use-package meow-tree-sitter
  :when (treesit-available-p)
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

;; Reset GC after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; Startup time display
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
(defun lkn-elisp-setup ()
  "Setup function to be called before all Emacs Lisp buffers."
  (setq-local lkn/elisp-imenu-expressions
              `(("Section" "^[ \t]*;;;*\\**[ \t]+\\([^\n]+\\)" 1)
                ("Evil commands" "^\\s-*(evil-define-\\(?:command\\|operator\\|motion\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                ("Unit tests" "^\\s-*(\\(?:ert-deftest\\|describe\\) +\"\\([^\")]+\\)\"" 1)
                ("Package" "^\\s-*\\(?:;;;###package\\|(use-package?\\) +\\(\\_<[^ ()\n]+\\_>\\)" 1)
                ("Major modes" "^\\s-*(define-derived-mode +\\([^ ()\n]+\\)" 1)
                ("Modeline Segment" "(lkn-modeline-defsegment[[:space:]\n]+\\([^[:space:]\n)]+\\)" 1)
                ("Minor modes" "^\\s-*(define-\\(?:global\\(?:ized\\)?-minor\\|generic\\|minor\\)-mode +\\([^ ()\n]+\\)" 1)
                ("Advice" "^\\s-*(\\(?:def\\(?:\\(?:ine-\\)?advice!?\\)\\) +\\([^ )\n]+\\)" 1)
                ("Macros" "^\\s-*(\\(?:cl-\\)?def\\(?:ine-compile-macro\\|macro\\) +\\([^ )\n]+\\)" 1)
                ("Inline functions" "\\s-*(\\(?:cl-\\)?defsubst +\\([^ )\n]+\\)" 1)
                ("Functions" "^\\s-*(\\(?:cl-\\)?def\\(?:un\\|un\\*\\|method\\|generic\\) +\\([^ ,)\n]+\\)" 1)
                ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\|var\\(?:-local\\)?\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                ("Types" "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)))
  (dolist (expression lkn/elisp-imenu-expressions)
    (add-to-list 'imenu-generic-expression expression)))

(add-hook 'emacs-lisp-mode-hook #'lkn-elisp-setup)

(defun lkn-elisp--calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.

Intended as :override advice for `calculate-lisp-indent'.

Adapted from URL `https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/'."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with
  ;; `defvar` with it's value ommited, marking it special and only defining it
  ;; locally. So if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start)
             (beginning-of-defun))
            ((setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren. Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.  Indent under
                 ;; that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line except the
                      ;; first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      (or
                       ;; Align keywords in plists if each newline begins with
                       ;; a keyword. This is useful for "unquoted plist
                       ;; function" macros, like `map!' and `defhydra'.
                       (when-let ((first (elt state 1))
                                  (char (char-after (1+ first))))
                         (and (eq char ?:)
                              (ignore-errors
                                (or (save-excursion
                                      (goto-char first)
                                      ;; FIXME Can we avoid `syntax-ppss'?
                                      (when-let* ((parse-sexp-ignore-comments t)
                                                  (end (scan-lists (point) 1 0))
                                                  (depth (ppss-depth (syntax-ppss))))
                                        (and (re-search-forward "^\\s-*:" end t)
                                             (= (ppss-depth (syntax-ppss))
                                                (1+ depth)))))
                                    (save-excursion
                                      (cl-loop for pos in (reverse (elt state 9))
                                               unless (memq (char-after (1+ pos)) '(?: ?\())
                                               do (goto-char (1+ pos))
                                               for fn = (read (current-buffer))
                                               if (symbolp fn)
                                               return (function-get fn 'indent-plists-as-data)))))))

                       ;; Check for quotes or backquotes around.
                       (let ((positions (elt state 9))
                             (quotep 0))
                         (while positions
                           (let ((point (pop positions)))
                             (or (when-let (char (char-before point))
                                   (cond
                                    ((eq char ?\())
                                    ((memq char '(?\' ?\`))
                                     (or (save-excursion
                                           (goto-char (1+ point))
                                           (skip-chars-forward "( ")
                                           (when-let (fn (ignore-errors (read (current-buffer))))
                                             (if (and (symbolp fn)
                                                      (fboundp fn)
                                                      ;; Only special forms and
                                                      ;; macros have special
                                                      ;; indent needs.
                                                      (not (functionp fn)))
                                                 (setq quotep 0))))
                                         (cl-incf quotep)))
                                    ((memq char '(?, ?@))
                                     (setq quotep 0))))
                                 ;; If the spelled out `quote' or `backquote'
                                 ;; are used, let's assume
                                 (save-excursion
                                   (goto-char (1+ point))
                                   (and (looking-at-p "\\(\\(?:back\\)?quote\\)[\t\n\f\s]+(")
                                        (cl-incf quotep 2)))
                                 (setq quotep (max 0 (1- quotep))))))
                         (> quotep 0))))
                     ;; Containing sexp has nothing before this line except the
                     ;; first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's almost
                 ;; certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset or
      ;; if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment or it does not apply
                ;; to this argument, try to align a constant-symbol under the
                ;; last preceding constant symbol, if there is such one of the
                ;; last 2 preceding symbols, in the previous uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation where it
                     ;; begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace following an
                       ;; open paren. (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant as
                ;; defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (normal-indent))))))

(advice-add #'calculate-lisp-indent :override #'lkn-elisp--calculate-lisp-indent)

(defvar lkn-elisp--face nil)
;;;###autoload
(defun lkn-elisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
      (let ((ppss (save-excursion (syntax-ppss))))
        (cond ((nth 3 ppss)  ; strings
               (search-forward "\"" end t))
              ((nth 4 ppss)  ; comments
               (forward-line +1))
              ((let ((symbol (intern-soft (match-string-no-properties 0))))
                 (and (cond ((null symbol) nil)
                            ((eq symbol t) nil)
                            ((keywordp symbol) nil)
                            ((special-variable-p symbol)
                             (setq lkn-elisp--face 'font-lock-variable-name-face))
                            ((and (fboundp symbol)
                                  (eq (char-before (match-beginning 0)) ?\()
                                  (not (memq (char-before (1- (match-beginning 0)))
                                             (list ?\' ?\`))))
                             (let ((unaliased (indirect-function symbol)))
                               (unless (or (macrop unaliased)
                                           (special-form-p unaliased))
                                 (let (unadvised)
                                   (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                   (setq unaliased (indirect-function unadvised)))))
                                   unaliased)
                                 (setq lkn-elisp--face
                                       (if (subrp unaliased)
                                           'font-lock-constant-face
                                         'font-lock-function-name-face))))))
                      (throw 'matcher t)))))))
    nil))

(font-lock-add-keywords
 'emacs-lisp-mode
 (append `((lkn-elisp-highlight-vars-and-faces . lkn-elisp--face))))

(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(use-package elisp-def
  :hook (emacs-lisp-mode . elisp-def-mode))

(use-package macrostep)

;; TODO Fix the bug with this and elpaca
(use-package eros
  ;; TODO This is an ideal candiate for whatever "major mode hydra" thing we do
  :bind
  (:map emacs-lisp-mode-map
	("C-c C-i" . eros-inspect-last-result))
  :hook (emacs-lisp-mode . eros-mode))

(use-package redshank
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . redshank-mode))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 300)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(use-package doct
  :after org-capture
  :commands (doct)
  :config
  (defun org-capture-select-template (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face 'nerd-icons-red :v-adjust 0.01) "\tAbort")))))))

  (defun org-mks (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold nerd-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ((equal pressed "ESC") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))

  (defun org-capture/replace-brackets (link)
    (mapconcat
     (lambda (c)
       (pcase (key-description (vector c))
         ("[" "(")
         ("]" ")")
         (_ (key-description (vector c)))))
     link))

  (defun doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
          (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (declare-function doct-flatten-lists-in "doct" (groups))
  (defun doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
                                       (setf (nth 1 template) (concat (doct-icon-declaration-to-icon spec)
                                                                      "\t"
                                                                      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(doct-iconify-capture-templates))

  (setq org-capture-templates
        (doct `(("Home" :keys "h"
                 :icon ("nf-oct-home" :set "octicon" :color "cyan")
                 :file "Home.org"
                 :prepend t
                 :headline "Inbox"
                 :template ("* TODO %?"
                            "%i %a"))
                ("Work" :keys "w"
                 :icon ("nf-fa-building" :set "faicon" :color "yellow")
                 :file "Work.org"
                 :prepend t
                 :headline "Inbox"
                 :template ("* TODO %?"
                            "SCHEDULED: %^{Schedule:}t"
                            "DEADLINE: %^{Deadline:}t"
                            "%i %a"))
                ("Note" :keys "n"
                 :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                 :file "Notes.org"
                 :template ("* %?"
                            "%i %a"))
                ("Journal" :keys "j"
                 :icon ("nf-fa-calendar" :set "faicon" :color "pink")
                 :type plain
                 :function (lambda ()
                             (org-journal-new-entry t)
                             (unless (eq org-journal-file-type 'daily)
                               (org-narrow-to-subtree))
                             (goto-char (point-max)))
                 :template "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                 :jump-to-captured t
                 :immediate-finish t)
                ("Protocol" :keys "P"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* TODO %^{Title}"
                            "Source: %u"
                            "#+BEGIN_QUOTE"
                            "%i"
                            "#+END_QUOTE"
                            "%?"))
                ("Protocol link" :keys "L"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* TODO %?"
                            "[[%:link][%:description]]"
                            "Captured on: %U"))
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "silver")
                 :prepend t
                 :type entry
                 :headline "Inbox"
                 :template ("* %{keyword} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:keyword "")
                 :children (("Task" :keys "t"
                             :icon ("nf-oct-checklist" :set "octicon" :color "green")
                             :keyword "TODO"
                             :file "todo.org")
                            ("Note" :keys "n"
                             :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                             :keyword "%U"
                             :file "notes.org")))))))

(defun org-fold-core-region (from to flag &optional spec-or-alias)
  "Hide or show lines from FROM to TO, according to FLAG.
SPEC-OR-ALIAS is the folding spec or foldable element, as a symbol.
If SPEC-OR-ALIAS is omitted and FLAG is nil, unfold everything in the region."
  (let ((spec (org-fold-core-get-folding-spec-from-alias spec-or-alias)))
    (when spec (org-fold-core--check-spec spec))
    (with-silent-modifications
      (org-with-wide-buffer
       ;; Arrange face property of newlines after all the folds
       ;; between FROM and TO to match the first character before the
       ;; fold; not the last as per Emacs defaults.  This makes
       ;; :extend faces span past the ellipsis.
       ;; See bug#65896.
       (if flag ; folding
           (when (equal ?\n (char-after to))
             (put-text-property to (1+ to) 'face (get-text-property from 'face)))
         ;; unfolding
         (dolist (region (org-fold-core-get-regions :from from :to to :specs spec))
           (when (equal ?\n (char-after (cadr region)))
             (font-lock-flush (cadr region) (1+ (cadr region))))))
       (when (eq org-fold-core-style 'overlays)
         (if org-fold-core--keep-overlays
             (mapc
              (lambda (ov)
                (when (or (not spec)
                          (eq spec (overlay-get ov 'invisible)))
                  (when (and org-fold-core--isearch-active
                             (overlay-get ov 'invisible)
                             (org-fold-core-get-folding-spec-property
                              (overlay-get ov 'invisible) :isearch-open))
                    (when (overlay-get ov 'invisible)
                      (overlay-put ov 'org-invisible (overlay-get ov 'invisible)))
                    (overlay-put ov 'invisible nil)
                    (when org-fold-core--isearch-active
                      (cl-pushnew ov org-fold-core--isearch-overlays)))))
              (overlays-in from to))
           (remove-overlays from to 'org-invisible spec)
           (remove-overlays from to 'invisible spec)))
       (if flag
	   (if (not spec)
               (error "Calling `org-fold-core-region' with missing SPEC")
             (if (eq org-fold-core-style 'overlays)
                 ;; Use `front-advance' since text right before to the beginning of
                 ;; the overlay belongs to the visible line than to the contents.
                 (let ((o (make-overlay from to nil
                                        (org-fold-core-get-folding-spec-property spec :front-sticky)
                                        (org-fold-core-get-folding-spec-property spec :rear-sticky))))
                   (when org-fold-core--isearch-active
                     (push o org-fold-core--isearch-overlays))
                   (overlay-put o 'evaporate t)
                   (overlay-put o (org-fold-core--property-symbol-get-create spec) spec)
                   (overlay-put o 'invisible spec)
                   ;; Preserve priority.
                   (overlay-put o 'priority (length (member spec (org-fold-core-folding-spec-list))))
                   (overlay-put o 'isearch-open-invisible #'org-fold-core--isearch-show)
                   (overlay-put o 'isearch-open-invisible-temporary #'org-fold-core--isearch-show-temporary))
	       (put-text-property from to (org-fold-core--property-symbol-get-create spec) spec)
	       (put-text-property from to 'isearch-open-invisible #'org-fold-core--isearch-show)
	       (put-text-property from to 'isearch-open-invisible-temporary #'org-fold-core--isearch-show-temporary)
               (when (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
                 ;; If the SPEC has highest priority, assign it directly
                 ;; to 'invisible property as well.  This is done to speed
                 ;; up Emacs redisplay on huge (Mbs) folded regions where
                 ;; we don't even want Emacs to spend time cycling over
                 ;; `char-property-alias-alist'.
                 (when (eq spec (caar org-fold-core--specs)) (put-text-property from to 'invisible spec)))))
         (if (not spec)
             (mapc (lambda (spec) (org-fold-core-region from to nil spec)) (org-fold-core-folding-spec-list))
           (when (and (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
                      (eq org-fold-core-style 'text-properties))
             (when (eq spec (caar org-fold-core--specs))
               (let ((pos from))
                 (while (< pos to)
                   (if (eq spec (get-text-property pos 'invisible))
                       (let ((next (org-fold-core-next-folding-state-change spec pos to)))
                         (remove-text-properties pos next '(invisible t))
                         (setq pos next))
                     (setq pos (next-single-char-property-change pos 'invisible nil to)))))))
           (when (eq org-fold-core-style 'text-properties)
	     (remove-text-properties from to (list (org-fold-core--property-symbol-get-create spec) nil)))))))))

(setopt org-hide-emphasis-markers t)

(setopt org-startup-with-inline-images t
        org-image-actual-width 600)

(setopt org-archive-location ".archive/Archive_%s::")

(setopt org-src-preserve-indentation t)       ;; This should ALWAYS be set

(setopt org-directory "~/Nextcloud/org"
        org-agenda-files '("~/Nextcloud/org/Home.org" "~/Nextcloud/org/Work.org" "~/Nextcloud/org/Notes.org"))

(setopt org-use-property-inheritance t)

(setopt org-cycle-separator-lines -1
	org-ellipsis " ▾")

(setopt org-startup-folded 'content)

(use-package jinx
  :init (global-jinx-mode)
  :custom
  (jinx-languages "en_GB")
  (jinx-include-modes '(text-mode prog-mode))
  (jinx-include-faces
   '((prog-mode font-lock-doc-face)
     (conf-mode font-lock-comment-face)))
  (jinx-exclude-regexps
   '((t "[A-Z]+\\>"
      "\\<[[:upper:]][[:lower:]]+\\>"
      "\\w*?[0-9\.'\"-]\\w*"
      "[a-z]+://\\S-+"
      "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?")))
  :bind
  (("M-$" . jinx-correct)))

(use-package dockerfile-mode
  :custom
  (dockerfile-mode-command (if IS-LINUX "podman" "docker")))

(require 'lkn-modeline)
(with-eval-after-load 'lkn-modeline
  (setopt mode-line-compact nil)
  (setq-default mode-line-format
                '("%e"
                  lkn-modeline-macro
                  lkn-modeline-narrow
                  lkn-modeline-remote
                  " "
                  lkn-modeline-meow-mode
                  lkn-modeline-major-mode
                  " "
                  lkn-modeline-buffer-name
                  " "
                  lkn-modeline-git-branch
                  " "
                  lkn-modeline-flymake))
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(require 'lkn-tab-bar)
(with-eval-after-load 'lkn-tab-bar
  (customize-set-variable 'global-mode-string '((:eval (lkn-tab-bar--workspaces)) " "))
  (customize-set-variable 'tab-bar-format '(tab-bar-format-global))
  (customize-set-variable 'tab-bar-mode t))
