;;; lkn-ui.el -- Without doubt the most important module -*- lexical-binding: t; svg-tag-action-at-point: edit -*-
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
;;; Anything related to the UI.
;;;
;;; That's quite a loose definition, so what's here tends to be pretty vague.
;;; Code:

(use-package svg-lib
  :ensure (:host github :repo "rougier/svg-lib"))

(use-package svg-tag-mode
  :when (display-graphic-p)
  :hook (prog-mode . svg-tag-mode)
  :hook (text-mode . svg-tag-mode)
  :init
  (defun svg-lib-nerdfont-tag (icon text &rest args)
    "Create a single tag with an ICON section and a TEXT section.
ARGS are style properties that affect the whole tag, with special handling for:
  :icon-foreground - foreground color for the icon part
  :icon-background - background color for the icon part"
    (let* ((style (apply #'svg-lib-style (svg-lib-style-default--get) args))
           (font-family (plist-get style :font-family))
           (base-font-size (plist-get style :font-size))
           (icon-font-size (* base-font-size 1.5))
           (text-font-size base-font-size)
           (font-weight (plist-get style :font-weight))
           (text-foreground (or (plist-get args :foreground)
                                (plist-get style :foreground)))
           (text-background (or (plist-get args :background)
                                (plist-get style :background)))
           (icon-foreground (or (plist-get args :icon-foreground)
                                text-foreground))
           (icon-background (or (plist-get args :icon-background)
                                text-background))
           (txt-char-width (window-font-width))
           (txt-char-height (window-font-height))
           (icon-width (* txt-char-width 2))
           (text-width (* (+ (length text) 1) txt-char-width))
           (total-width (+ icon-width text-width))
           (height txt-char-height)
           (radius 3)
           (svg (svg-create total-width height)))

      (let ((clip-path (svg-clip-path svg :id "rounded-corners")))
        (svg-rectangle clip-path 0 0 total-width height
                       :rx radius))

      (svg-rectangle svg 0 0 icon-width height
                     :fill icon-background
                     :clip-path "url(#rounded-corners)")

      (svg-rectangle svg icon-width 0 text-width height
                     :fill text-background
                     :clip-path "url(#rounded-corners)")

      (svg-text svg icon
                :font-family font-family
                :font-weight font-weight
                :font-size icon-font-size
                :fill icon-foreground
                :x (/ icon-width 2)
                :y (* height 0.82)
                :text-anchor "middle")

      (svg-text svg text
                :font-family font-family
                :font-weight font-weight
                :font-size text-font-size
                :fill text-foreground
                :x (+ icon-width (/ text-width 2))
                :y (* height 0.82)
                :text-anchor "middle")

      (svg-lib--image svg :ascent 'center)))

  (defvar svg-tag-cache (make-hash-table :test 'equal))

  (defun svg-tag-make-with-cache (tag &rest args)
    (with-memoization (gethash `(,(substring-no-properties tag) ,@args) svg-tag-cache)
      (let* ((tag (string-trim tag))
             (beg (or (plist-get args :beg) 0))
             (end (or (plist-get args :end) nil))
             (args (svg-tag--plist-delete args 'font-weight)))
        (apply #'svg-lib-tag (substring tag beg end) nil
               :font-weight 'regular
               args))))

  (defun svg-nerdfont-tag-make-with-cache (icon tag &rest args)
    (with-memoization (gethash `(,(substring-no-properties icon) ,(substring-no-properties tag) ,@args) svg-tag-cache)
      (let* ((tag (string-trim tag))
             (beg (or (plist-get args :beg) 0))
             (end (or (plist-get args :end) nil))
             (args (svg-tag--plist-delete args 'font-weight)))
        (apply #'svg-lib-nerdfont-tag icon (substring tag beg end)
               :font-weight 'regular
               args))))
  :custom
  ;; TODO: CRM457-2332: Test
  ;; :nocov:
  (svg-tag-tags
   '(("TODO:" . ((lambda (tag)
                   (svg-tag-make-with-cache
                    "TODO"
                    :foreground (cdr (assoc "TODO" hl-todo-keyword-faces))))))
     (":nocov:" . ((lambda (tag)
                     (svg-nerdfont-tag-make-with-cache
                      (nerd-icons-faicon "nf-fae-ruby") "SimpleCov ignore"
                      :font-weight 900
                      :background (doom-color 'bg-alt)
                      :icon-background "white"
                      :icon-foreground "#9b111e"))))
     ("CRM457-[0-9]+:" . ((lambda (tag)
                            (svg-nerdfont-tag-make-with-cache
                             (nerd-icons-mdicon "nf-md-jira") tag
                             :font-weight 900
                             :background (doom-color 'bg-alt)
                             :icon-background "white"
                             :icon-foreground "#0052CC"
                             :end -1))
                          (lambda (&rest args)
                            (interactive)
                            (when-let* ((code (substring-no-properties
                                               (thing-at-point 'symbol))))
                              (when jiralib-url
                                (browse-url-default-browser
                                 (url-recreate-url
                                  (url-generic-parse-url (concat jiralib-url "/browse/" code))))))))))))

(use-package mixed-pitch
  :when (display-graphic-p)
  :hook (org-mode . mixed-pitch-mode)
  :hook (LaTeX-mode . mixed-pitch-mode)
  :hook (markdown-mode . mixed-pitch-mode)
  :hook (gfm-mode . mixed-pitch-mode)
  :hook (Info-mode . mixed-pitch-mode)
  :hook (rst-mode . mixed-pitch-mode)
  :hook (adoc-mode . mixed-pitch-mode)
  :hook (mixed-pitch-mode . update-corfu-faces)
  :init
  ;; Ensure the font is correct for mixed-pitch modes
  (defun update-corfu-faces ()
    (with-eval-after-load 'corfu
      (set-face-attribute 'corfu-current nil :font lkn-default-font)
      (set-face-attribute 'corfu-default nil :font lkn-default-font))))

(use-package hide-mode-line)

(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :hook (helpful-mode . (lambda ()  (display-line-numbers-mode -1)))
  :custom
  (apropos-do-all t)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ("C-h '"                   . describe-char)
         ("C-h F"                   . describe-face)
         ("C-h C-k"                 . describe-keymap)
         ("C-h C-f"                 . describe-buffer-faces))
  :config
;;;###autoload
  (defun describe-buffer-faces (&optional buffer)
    "Display list of font faces used in BUFFER with examples and navigation.

If not specified, or called interactively, BUFFER defaults to `current-buffer'.
Each face entry shows an example of text with that face and buttons to
navigate to occurrences in the buffer."
    (interactive)
    (let* ((working-buffer (or buffer (current-buffer)))
           (face-data (describe-buffer-faces--collect-data working-buffer)))

      (help-setup-xref (list 'describe-buffer-faces working-buffer)
                       (called-interactively-p 'interactive))

      (with-help-window (help-buffer)
        (insert (format "Faces found in buffer '%s':\n\n" (buffer-name working-buffer)))
        (dolist (entry (sort face-data (lambda (a b)
                                          (> (plist-get a :count)
                                             (plist-get b :count)))))
          (describe-buffer-faces--insert-entry working-buffer entry)))))

  (defun describe-buffer-faces--collect-data (buffer)
    "Collect face data from BUFFER.
Returns a list of plists with :face, :first-pos, :count, and :example."
    (let ((face-table (make-hash-table :test 'equal)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (< (point) (point-max))
            (when-let* ((face (get-text-property (point) 'face)))
              (let* ((pos (point))
                     (entry (or (gethash face face-table)
                               (puthash face (list :face face :first-pos pos :count 0)
                                       face-table))))
                (cl-incf (plist-get entry :count))
                (unless (plist-get entry :example)
                  (let ((end (next-property-change pos nil (point-max))))
                    (plist-put entry :example
                              (buffer-substring-no-properties pos (min end (+ pos 50))))))))
            (goto-char (next-property-change (point) nil (point-max))))))
      (hash-table-values face-table)))

  (defun describe-buffer-faces--insert-entry (buffer entry)
    "Insert a formatted ENTRY for BUFFER in the help window."
    (let ((face (plist-get entry :face))
          (first-pos (plist-get entry :first-pos))
          (count (plist-get entry :count))
          (example (plist-get entry :example)))

      ;; Face name and count
      (help-insert-xref-button (format "  %s" face) 'help-face face)
      (insert (format " (%d occurrence%s)\n" count (if (> count 1) "s" "")))

      ;; Example text
      (when example
        (insert "    Example: "
                (propertize (truncate-string-to-width example 60 nil nil "…") 'face face)
                "\n"))

      ;; Navigation buttons
      (insert "    ")
      (describe-buffer-faces--insert-button "Next" buffer face 'next)
      (insert " ")
      (describe-buffer-faces--insert-button "Previous" buffer face 'prev)
      (insert " ")
      (describe-buffer-faces--insert-button "First" buffer face first-pos)
      (insert "\n\n")))

  (defun describe-buffer-faces--insert-button (label buffer face action)
    "Insert a navigation button with LABEL for FACE in BUFFER.
ACTION is either next, prev, or a position number."
    (insert-button (format "[%s]" label)
                  'action (lambda (_)
                            (pop-to-buffer buffer)
                            (if (numberp action)
                                (goto-char action)
                              (describe-buffer-faces--find-face face action))
                            (recenter))
                  'follow-link t
                  'help-echo (format "Jump to %s occurrence in buffer"
                                    (if (numberp action) "first" (symbol-name action)))))

  (defun describe-buffer-faces--find-face (face direction)
    "Find occurrence of FACE in DIRECTION (next or prev), wrapping if needed."
    (let* ((forward (eq direction 'next))
           (start (point))
           (limit (if forward (point-max) (point-min)))
           (move-fn (if forward #'next-property-change #'previous-property-change))
           (compare (if forward #'< #'>)))
      ;; Search in primary direction
      (goto-char (funcall move-fn (point) nil limit))
      (while (and (funcall compare (point) limit)
                  (not (equal (get-text-property (point) 'face) face)))
        (goto-char (funcall move-fn (point) nil limit)))
      ;; Wrap around if we didn't find it
      (when (funcall compare limit (point))
        (goto-char (if forward (point-min) (point-max)))
        (while (and (funcall compare (point) start)
                    (not (equal (get-text-property (point) 'face) face)))
          (goto-char (funcall move-fn (point) nil limit)))))))

(use-package doom-themes
  :demand t
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :hook (after-load-theme . reset-theme)
  :init
  (defun reset-theme ()
    "Ensure that we load the theme correctly.
We do this by disabling all other themes then loading ours."
    (let ((themes custom-enabled-themes)
          (after-load-theme-hook nil))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme (car themes) t))))

(use-package solaire-mode
  :after doom-themes
  :init (solaire-global-mode))

(use-feature which-key
  :init
  (which-key-mode)
  ;; Credit to
  ;; <https://karthinks.com/software/it-bears-repeating/#adding-a-hydra-like-prompt-to-repeat-mode>
  ;; Custom repeat-echo-function using which-key
  (defun repeat-echo-which-key (keymap)
    "Show repeat bindings using which-key for KEYMAP."
    (if keymap
        (let ((which-key-allow-multiple-replacements t)
              (which-key-replacement-alist
               (let (replacements)
                 (map-keymap
                  (lambda (_key cmd)
                    (when-let* ((hint (and (symbolp cmd)
                                          (get cmd 'repeat-hint))))
                      (push `((nil . ,(regexp-quote (symbol-name cmd))) . (nil . ,hint))
                            replacements)))
                  keymap)
                 (append which-key-replacement-alist replacements))))
          (which-key--hide-popup)
          (which-key--update)
          (which-key--create-buffer-and-show nil keymap))
      (which-key--hide-popup)))
  :custom
  (repeat-echo-function #'repeat-echo-which-key)
  (which-key-idle-delay 1.0)
  (which-key-separator " → ")
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-max-description-length 30)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot 1)
  (which-key-allow-multiple-replacements t)
  (which-key-ellipsis "…")
  :config
  (which-key-setup-side-window-bottom))

(use-package which-key-posframe
  :when (display-graphic-p)
  :after which-key
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-window-bottom-left-corner)
  :config
  (which-key-posframe-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init
  (setq corfu-margin-formatters '(nerd-icons-corfu-formatter)))

(use-package nerd-icons
  :defer 1
  :config
  (when (and (not (member "Symbols Nerd Font Mono" (font-family-list)))
             (display-graphic-p))
    (nerd-icons-install-fonts t))
  (set-fontset-font t 'unicode (font-spec :family "Symbols Nerd Fonts Mono") nil 'prepend))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package colorful-mode
  :init (global-colorful-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package popper
  ;; :after perspective
  :init (popper-mode)
  :hook (popper-mode . popper-echo-mode)
  :bind (("M-`"   . popper-toggle)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-mode-line
   '(:eval
     (propertize "  " 'face 'mode-line-emphasis)))
  (popper-group-function #'popper-group-by-project)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*rails\\*"
     "\\*Claude\\*"
     "\\*Async Shell Command\\*"
     "-gptel\\*"
     eshell-mode
     help-mode
     helpful-mode
     Info-mode
     "\\*\\(vterm\\|vterminal\\).*\\*"
     vterm-mode
     org-journal-mode
     "\\*Org Select\\*"
     rspec-compilation-mode
     cider-repl-mode
     compilation-mode)))

(use-package doom-modeline
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-github nil)
  (doom-modeline-minor-modes t)
  (auto-revert-check-vc-info t)
  (doom-modeline-vcs-max-length 60)
  :config
  (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
      (doom-modeline-def-modeline 'main
      '(matches bar modals window-number persp-name buffer-info remote-host debug vcs media-info pdf-pages)
      '(check compilation objed-state process github mu4e grip gnus misc-info repl lsp minor-modes)))

(use-feature lkn-modeline
  :disabled t
  :custom
  (mode-line-compact nil)
  (mode-line-format '("%e"
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
  :config
  (let ((subtle (face-foreground 'shadow)))
    (custom-set-faces
     `(mode-line ((t :background unspecified :box unspecified :overline ,subtle)))
     `(mode-line-inactive ((t :background unspecified :foreground ,subtle :box unspecified :overline ,subtle))))))

(use-feature lkn-tab-bar
  :disabled t
  :after (doom-themes)
  :custom
  (tab-bar-format '(lkn-tab-bar-format-align-center
                    lkn-tab-bar-workspaces-format
                    tab-bar-format-align-right))
  (tab-bar-mode t)
  :init (require 'lkn-tab-bar))

(use-package trashed
  :after dired
  :disabled IS-MAC
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package dape
  :bind-keymap ("C-x C-a" . dape-global-map)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (elpaca-after-init . dape-breakpoint-load)
  (elpaca-after-init . dape-breakpoint-global-mode)
  (dape-compile . kill-buffer)
  :custom
  (dape-inlay-hints t)
  :config
  (setq dape-configs
        (cons
         `(rdbg-rails
           modes (ruby-mode ruby-ts-mode web-mode)
           :type "ruby"
           :request "attach"
           port (if (file-exists-p (expand-file-name ".env.development.local" (project-root (project-current))))
                    (string-to-number (dotenv-get "WEB_DEBUG_PORT" (expand-file-name ".env.development.local" (project-root (project-current)))))
                  :autoport)
           :localfs t)
         (cl-remove-if (lambda (cfg) (eq (car-safe cfg) 'rdbg-rails))
                       dape-configs))))

(use-package ace-window
  :custom
  (aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  (aw-scope 'frame)
  :bind
  (:map global-map
        ("M-o" . ace-window)))

(use-package spacious-padding
  :when (display-graphic-p)
  :hook (elpaca-after-init . spacious-padding-mode)
  :bind
  ("C-c C-t S" . spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line
   '(:mode-line-active tab-bar
     :mode-line-inactive shadow))
  (spacious-padding-widths
   '(:internal-border-width 10
     :header-line-width 4
     :mode-line-width 6
     :tab-width 4
     :right-divider-width 30
     :scroll-bar-width 8
     :fringe-width 8)))

(use-package ultra-scroll
  :ensure (:host github :repo "jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 3)
  (scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package fancy-compilation
  :custom
  (fancy-compilation-term "xterm-256color")
  :hook (compilation-mode . fancy-compilation-mode)
  :after compile)

(use-package beacon
  :diminish (beacon-mode)
  :config (beacon-mode))

(use-package diredfl
  :init (diredfl-global-mode))

(provide 'lkn-ui)
;;; lkn-ui.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
