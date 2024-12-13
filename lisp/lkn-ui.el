;;; lkn-ui.el -- Without doubt the most important module -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2024 Ellis Kenyő

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

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :hook (LaTeX-mode . mixed-pitch-mode)
  :hook (markdown-mode . mixed-pitch-mode)
  :hook (gfm-mode . mixed-pitch-mode)
  :hook (Info-mode . mixed-pitch-mode)
  :hook (rst-mode . mixed-pitch-mode)
  :hook (adoc-mode . mixed-pitch-mode))

(use-package hide-mode-line)

(use-package helpful
  :hook (helpful-mode . visual-line-mode)
  :init
  (setopt apropos-do-all t)
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

(use-package doom-themes
  :demand t
  :config
  (setopt doom-themes-enable-bold t
          doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  :hook (after-load-theme . reset-theme)
  :hook (after-load-theme . update-default-colours)
  :hook (mixed-pitch-mode . update-corfu-faces)
  :init
  (defun reset-theme ()
    "Ensure that we load the theme correctly.
We do this by disabling all other themes then loading ours."
    (let ((themes custom-enabled-themes)
	  (after-load-theme-hook nil))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme (car themes) t)))

  (defun update-default-colours ()
    "Update the base colours Emacs uses before anything loads."
    (with-temp-file (expand-file-name ".emacs-colours.el" user-emacs-directory)
      (insert ";; Autogenerated from hook, don't update\n")
      (insert (format ";; Colours for %s\n" (car custom-enabled-themes)))
      (insert (format "(set-face-attribute 'fringe nil :background \"%s\" :foreground \"%s\")\n" (face-attribute 'default :background) (face-attribute 'default :foreground)))))
  
  ;; Ensure the font is correct for mixed-pitch modes
  (defun update-corfu-faces ()
    (with-eval-after-load 'corfu
      (set-face-attribute 'corfu-current nil :font lkn-default-font)
      (set-face-attribute 'corfu-default nil :font lkn-default-font))))

(use-package solaire-mode
  :after doom-themes
  :init (solaire-global-mode))

;; Disabled for now as I debate if it's needed
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setopt which-key-idle-delay 1.0
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
  (which-key-setup-side-window-bottom))

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

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

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
     (propertize " 󰍡 " 'face 'mode-line-emphasis)))
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
    '(matches bar modals workspace-name window-number persp-name buffer-info remote-host debug vcs media-info info-nodes pdf-pages)
    '(compilation objed-state process minor-modes github mu4e grip gnus check misc-info repl lsp " ")))

(use-package flymake-popon
  :diminish flymake-popon-mode
  :custom
  (flymake-popon-diagnostic-formatter #'lkn/flymake-popon-format-diagnostic)
  :custom-face
  (flymake-popon ((t (:inherit corfu-default))))
  (flymake-popon-posframe-border ((t (:foreground "white"))))
  :hook (flymake-mode . flymake-popon-mode)
  :init
  (defun lkn/flymake-popon-format-diagnostic (diagnostic)
    "Format DIAGNOSTIC into a modern, visually appealing message.
Adds icons, proper spacing, and clean line wrapping for readability."
    (let* ((diag-type (flymake-diagnostic-type diagnostic))
           (diag-face (flymake--lookup-type-property diag-type 'mode-line-face))
           (border-color (face-attribute diag-face :foreground nil t))
           (prefix-icon (pcase diag-type
			  (:error (nerd-icons-faicon "nf-fa-times"))
			  (:warning (nerd-icons-faicon "nf-fa-exclamation_triangle"))
			  (:note (nerd-icons-faicon "nf-fa-info_circle"))
			  (_ (nerd-icons-faicon "nf-fa-question_circle"))))
           ;; Context and position calculation
           (context-data 
            (when-let* ((buf (flymake-diagnostic-buffer diagnostic))
			(beg (flymake-diagnostic-beg diagnostic))
			((buffer-live-p buf)))
              (with-current-buffer buf
		(save-excursion
                  (goto-char beg)
                  (let* ((line (buffer-substring (line-beginning-position)
						 (line-end-position)))
			 (trimmed-line (string-trim line))
			 (left-trim-len (- (length line)
					   (length (string-trim-left line))))
			 (orig-col (- beg (line-beginning-position)))
			 (offset (- orig-col left-trim-len))
			 (len (- (flymake-diagnostic-end diagnostic)
				 (flymake-diagnostic-beg diagnostic))))
                    ;; Add bold to the diagnostic portion
                    (put-text-property offset (+ offset len) 'face
                                       `(:inherit ,diag-face :weight bold)
                                       trimmed-line)
                    (list trimmed-line offset))))))
           (context (car context-data))
           (error-col (cadr context-data))
           (message (car (split-string (flymake-diagnostic-text diagnostic) "[\n\r]" t "[ \t]+")))
         ;; Title case the first word of the message
         (message (if (string-match "^\\([a-z]\\)\\([^ ]*\\)\\(.*\\)" message)
                     (concat (upcase (match-string 1 message))
                            (match-string 2 message)
                            (match-string 3 message))
                   message)))
    
    (concat
     (propertize "╭─╮\n│ " 'face `(:foreground ,border-color))
     (propertize prefix-icon 'face diag-face) " "
     (propertize message 'face diag-face) "\n"
     (when context
       (concat (propertize "│   " 'face `(:foreground ,border-color))
               context "\n"))
     (when error-col
       (concat (propertize "╰" 'face `(:foreground ,border-color))
              (propertize (make-string (+ error-col 3) ?─)
                         'face `(:foreground ,border-color))
              (propertize "╯" 'face `(:foreground ,border-color))))))))

;; Load the tab bar after elpaca is setup since we loosely depend on
;; some packages
(add-hook 'elpaca-after-init-hook
	  (lambda ()
	    (require 'lkn-tab-bar)
	    (with-eval-after-load 'lkn-tab-bar
	      (customize-set-variable 'global-mode-string '((:eval (lkn-tab-bar--workspaces)) " "))
	      (customize-set-variable 'tab-bar-format '(tab-bar-format-global))
	      (customize-set-variable 'tab-bar-mode t))))

(use-package kubel
  :after vterm
  :config
  (kubel-vterm-setup))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove)))

(use-package trashed
  :after dired
  :disabled IS-MAC
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package eldoc-box
  :after eldoc
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-only-multi-line t)
  :custom-face
  (eldoc-box-body ((t (:font ,lkn-variable-pitch-font)))))

;; (use-package dape
;;   :hook
;;   (kill-emacs . dape-breakpoint-save)
;;   (after-init . dape-breakpoint-load)
;;   (after-init . dape-breakpoint-global-mode)
;;   (dape-compile . kill-buffer)
;;   :custom
;;   (dape-inlay-hints t))

(provide 'lkn-ui)
;;; lkn-ui.el ends here
