;;; lkn-completion.el -- VEMCO gives you wiiings -*- lexical-binding: t -*-
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
;;; VEMCO stack, Corfu, and a bunch of other minad-y packages
;;; Code:


(use-package vertico
  :custom
  (vertico-resize nil)
  (vertico-count 17)
  (vertico-cycle t)
  (completion-in-region-function #'consult-completion-in-region)
  :custom-face
  (vertico-quick1 ((t (:foreground unspecified :background unspecified :inherit (outline-1 bold)))))
  (vertico-quick2 ((t (:foreground unspecified :background unspecified :inherit (outline-2 bold)))))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :hook (vertico-mode . file-name-shadow-mode)
  :hook (vertico-mode . vertico-mouse-mode)
  :bind
  ("C-c '" . vertico-repeat)
  :init
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

(use-package embark-vc
  :after (embark magit forge)
  :ensure (:host github :repo "elken/embark-vc")
  :custom
  (embark-vc-review-provider 'pr-review))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode)
  :config
  (setq marginalia-censor-variables nil)

  (defun marginalia--annotate-local-file (cand)
    "Improved local file annotations for CAND, coloured based on recency."
    (when-let* ((attrs (file-attributes (substitute-in-file-name
                                         (marginalia--full-candidate cand))
                                       'integer)))
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
           (blend-amount (max 0.0 (min 1.0 (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   blend-amount)))
      (when (and (stringp color) (not (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)))
        (setq color (face-attribute 'marginalia-date :foreground nil t)))
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    "Improved local file annotations, coloured based on SIZE."
    (let* ((size-index (max 0.0 (min 1.0 (/ (log (+ 1 size)) 7.0))))
           (color (if (< size-index 1.0)
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange 0.5))))
      (when (and (stringp color) (not (string-match-p "^#[0-9a-fA-F]\\{6\\}$" color)))
        (setq color "orange"))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

(use-package consult
  :demand t
  :custom
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (imenu-max-item-length 'unlimited)
  :bind
  (([remap apropos]                       . consult-apropos)
   ([remap imenu]                         . consult-imenu)
   ("M-g I"                               . consult-imenu-multi)
   ([remap load-theme]                    . consult-theme)
   ([remap man]                           . consult-man)
   ([remap recentf-open-files]            . consult-recent-file)
   ([remap switch-to-buffer]              . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
   ("C-c /"                               . consult-ripgrep))
  :config
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

(use-package orderless
  :custom
  (completion-pcm-leading-wildcard t)
  (completion-styles '(orderless partial-completion))
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless))
                                   (eglot-capf (styles orderless))))
  (orderless-component-separator "[ &]")
  (completion-category-defaults nil)
  :config
  (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch))

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
  :ensure (:host github :repo "elken/package-capf")
  :after cape
  :hook (emacs-lisp-mode . lkn/corfu--set-emacs-lisp-capfs)
  :init
  (defun lkn/corfu--set-emacs-lisp-capfs ()
    (make-local-variable 'completion-at-point-functions)
    (add-to-list 'completion-at-point-functions #'package-capf)))

(use-package corfu
  :demand t
  :bind
  (:map corfu-map
        ("C-SPC" . corfu-insert-separator)
        ("C-n"   . corfu-next)
        ("C-p"   . corfu-previous)
        ("C-q"   . corfu-quick-insert))
  :hook
  (corfu-mode . corfu-history-mode)
  (corfu-mode . corfu-echo-mode)
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-on-exact-match nil)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (corfu-min-width 50)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-preview-current nil)
  :custom-face
  (corfu-quick1 ((t (:foreground unspecified :background unspecified :inherit vertico-quick1))))
  (corfu-quick2 ((t (:foreground unspecified :background unspecified :inherit vertico-quick2))))
  :init
  (global-corfu-mode)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(provide 'lkn-completion)
;;; lkn-completion.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
