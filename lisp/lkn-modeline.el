;;; lkn-modeline.el --- lknmacs -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright ©️ 2022-2025 Ellis Kenyő

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;; Commentary:
;;; My custom modeline heavily inspired from
;;; <https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d>.
;;; Currently not in use, but should serve as a good reference for
;;; those wanting to make their own.
;;; Code:

(defgroup lkn-modeline nil
  "Settings related to my custom modeline."
  :group 'mode-line)

(defgroup lkn-modeline-faces nil
  "Faces for my custom modeline."
  :group 'lkn-modeline)

(defcustom lkn-modeline-max-length 40
  "Maximum length of strings before they get trimmed."
  :type 'natnum)

(defface lkn-modeline-background
  '((t :background "#292e39" :foreground "white" :inherit bold))
  "Face for the background of the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-remote
  '((t (:inherit (warning bold))))
  "Face for remote hostname in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-meow-normal
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for Emacs state in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-meow-insert
  '((t (:inherit (font-lock-string-face bold))))
  "Face for insert state in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-meow-motion
  '((t (:inherit (font-lock-doc-face bold))))
  "Face for motion state in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-meow-keypad
  '((t (:inherit (font-lock-type-face bold))))
  "Face for keypad state in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-meow-beacon
  '((t (:inherit (font-lock-number-face bold))))
  "Face for beacon state in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git
  '((t (:inherit (success bold))))
  "Face for git branch in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-modified
  '((t (:inherit (warning bold))))
  "Face for git branch in the modeline."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git
  '((t (:inherit success)))
  "Face for git branch in mode line."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-staged
  '((t (:inherit success)))
  "Face for staged changes indicator in mode line."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-unstaged
  '((t (:inherit warning)))
  "Face for unstaged changes indicator in mode line."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-untracked
  '((t (:inherit error)))
  "Face for untracked files indicator in mode line."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-merge
  '((t (:inherit font-lock-number-face)))
  "Face for merge in progress indicator in mode line."
  :group 'lkn-modeline-faces)

(defface lkn-modeline-git-rebase
  '((t (:inherit font-lock-type-face)))
  "Face for rebase in progress indicator in mode line."
  :group 'lkn-modeline-faces)

(defun lkn-modeline-trim (str)
  "If applicable, trim STR according to `lkn-modeline-string-max-length'."
  (if (and (< (window-total-width) split-width-threshold)
           (> (length str) lkn-modeline-max-length)
           (not (one-window-p :no-minibuffer)))
      (truncate-string-to-width str lkn-modeline-max-length 0 nil "...")
    str))

(defmacro lkn-modeline-defsegment (name &rest body)
  "Define a mode-line segment NAME to be loaded with BODY.
This macro is preferred since it also marks the needed variable as risky."
  `(progn
     (put ',name 'risky-local-variable t)
     (setq-default ,name
                   '(:eval
                     ,@body))))

(lkn-modeline-defsegment
 lkn-modeline-meow-mode
 (when (mode-line-window-selected-p)
   (let* ((tag meow--current-state)
          (state-name (format " %s " (thread-first
                                       tag
                                       symbol-name
                                       upcase))))
     (add-text-properties
      0
      (length state-name)
      `(face ,(intern-soft (format "lkn-modeline-meow-%s" (downcase (symbol-name tag)))))
      state-name)
     state-name)))

(lkn-modeline-defsegment
 lkn-modeline-narrow
 (when (and (mode-line-window-selected-p)
            (buffer-narrowed-p)
            (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
   (propertize "" 'face 'mode-line-highlight)))

(lkn-modeline-defsegment
 lkn-modeline-remote
 (when-let* ((host (file-remote-p default-directory 'host)))
   (propertize
    "   "
    'face 'lkn-modeline-remote
    'mouse-face 'mode-line-highlight
    'help-echo host)))

(defun lkn-modeline-buffer-faces ()
  "Return the correct faces based on the current buffer state."
  (let ((file (buffer-file-name)))
    (list
     (when (and file (buffer-modified-p))
       'warning)
     (when (mode-line-window-selected-p)
       'mode-line-buffer-id))))

(lkn-modeline-defsegment
 lkn-modeline-buffer-name
 (when-let* ((name (buffer-name)))
   (let ((name (if (project-current) (file-relative-name name (project-root (project-current))) name))
         (icon (and buffer-read-only ""))
         (echo-text (or (buffer-file-name) (format "No file associated.\n%s" default-directory))))
     (propertize
      name
      'face (lkn-modeline-buffer-faces)
      'mouse-face 'mode-line-highlight
      'help-echo echo-text))))

(lkn-modeline-defsegment
 lkn-modeline-major-mode
 (let* ((icon (nerd-icons-icon-for-buffer))
        (mode (format-mode-line mode-name))
        (help-echo (when (listp mode) (get-text-property (length (car mode-name)) 'help-echo mode))))
   (add-text-properties
    0
    (length icon)
    `(help-echo ,(format "Major Mode: %s%s" (substring-no-properties mode) (if help-echo (concat "\n" help-echo) ""))
      mouse-face mode-line-highlight)
    icon)
   icon))

(declare-function vc-git--symbolic-ref "vc-git" (file))

(lkn-modeline-defsegment
 lkn-modeline-git-branch
 (when (mode-line-window-selected-p)
   (when-let* ((branch (magit-get-current-branch)))
     (concat
      (propertize (nerd-icons-octicon "nf-oct-git_branch") 'face 'shadow)
      " "
      (propertize (truncate-string-to-width branch lkn-modeline-max-length)
                  'face 'lkn-modeline-git
                  'mouse-face 'mode-line-highlight)
      " "
      (when-let* ((state (cond
                          ((magit-merge-in-progress-p)
                           (cons (nerd-icons-codicon "nf-cod-git_merge") 'lkn-modeline-git-merge))
                          ((magit-rebase-in-progress-p)
                           (cons (nerd-icons-codicon "nf-cod-git_pull_request") 'lkn-modeline-git-rebase))
                          ((magit-anything-staged-p)
                           (cons (nerd-icons-octicon "nf-oct-diff_added") 'lkn-modeline-git-staged))
                          ((magit-anything-unstaged-p)
                           (cons (nerd-icons-octicon "nf-oct-diff_modified") 'lkn-modeline-git-unstaged))
                          ((magit-untracked-files)
                           (cons (nerd-icons-octicon "nf-oct-diff") 'lkn-modeline-git-untracked))
                          (t nil))))
        (propertize (car state) 'face (cdr state)))))))

(defun lkn-modeline-flymake--text (ht key)
  "Helper function to get all KEY diagnostics from HT."
  (when-let* ((value (gethash key ht)))
    (concat
     (propertize (nerd-icons-codicon "nf-cod-error")
                 'face `(:inherit ,key :height 1.2))
     " "
     (propertize
      (number-to-string value)
      'mouse-face 'mode-line-highlight)
     " ")))

(lkn-modeline-defsegment
 lkn-modeline-flymake
 (when (bound-and-true-p flymake-mode)
   (let* ((running (flymake-running-backends))
          (disabled (flymake-disabled-backends))
          (reported (flymake-reporting-backends))
          (all-disabled (and disabled (null running)))
          (some-waiting (cl-set-difference running reported))
          (warning-level (warning-numeric-level :warning))
          (note-level (warning-numeric-level :debug))
          (hash (make-hash-table :test #'equal)))
     (maphash (lambda (_b state)
                (cl-loop
                 for diag in (flymake--state-diags state) do
                 (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                (warning-numeric-level :error))))
                   (cond ((> severity warning-level) (cl-incf (gethash 'error hash 0)))
                         ((> severity note-level) (cl-incf (gethash 'warning hash 0)))
                         (t (cl-incf (gethash 'success hash 0)))))))
              flymake--state)
     (concat
      (lkn-modeline-flymake--text hash 'error)
      (lkn-modeline-flymake--text hash 'warning)
      (lkn-modeline-flymake--text hash 'success)))))

(provide 'lkn-modeline)
;;; lkn-modeline.el ends here
