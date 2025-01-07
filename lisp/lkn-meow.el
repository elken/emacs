;;; lkn-meow.el -- Vim is dead; long live vim -*- lexical-binding: t -*-
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
;;; Setup for my meow configuration that may or may not be
;;; in use currently.  It gets in the way sometimes, so this may be
;;; phased out.
;;; Code:

(defun lkn/toggle-case-dwim ()
  "Toggle case of active region or word at point."
  (interactive "*")
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text (when bounds
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (when text
      (delete-region (car bounds) (cdr bounds))
      (insert (apply #'string
                     (mapcar (lambda (char)
                               (if (eq (upcase char) char)
                                   (downcase char)
				 (upcase char)))
                             (string-to-list text)))))))

(defun lkn/meow-kill-dwim ()
  "DWIM delete that preserves the kill ring."
  (interactive)
  (cond
   ((region-active-p)
    (cond
     (paredit-mode (paredit-kill-region (region-beginning) (region-end)))
     (smartparens-strict-mode (sp-kill-region (region-beginning) (region-end)))
     (t (kill-region (region-beginning) (region-end))))
    (meow-cancel-selection))
   (paredit-mode
    (when-let ((char (char-after)))
      (paredit-forward-delete)
      (kill-new (char-to-string char))))
   (smartparens-strict-mode
    (when-let ((char (char-after)))
      (sp-delete-char 1)
      (kill-new (char-to-string char))))
   (t (kill-forward-chars 1))))

(defun lkn/meow-delete-dwim ()
  "DWIM delete that doesn't use the kill ring."
  (interactive)
  (cond
   ((region-active-p)
    (cond
     (paredit-mode (paredit-delete-region (region-beginning) (region-end)))
     (smartparens-strict-mode (sp-delete-region (region-beginning) (region-end)))
     (t (delete-region (region-beginning) (region-end))))
    (meow-cancel-selection))
   (paredit-mode
    (paredit-forward-delete))
   (smartparens-strict-mode
    (sp-delete-char 1))
   (t (delete-char 1))))

(defun lkn/meow-append-after ()
  "Behave more like Vim/Helix's `a'."
  (interactive)
  (if (region-active-p)
      (progn
	(goto-char (region-end))
	(meow-cancel-selection)))
  (forward-char)
  (meow-insert))

(defun lkn/goto-matching-paren ()
  "Go to the matching parenthesis if on parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-sexp 1))
        ((looking-back "\\s\)" 1) (backward-sexp 1))
        ;; Now, try the other direction
        ((looking-at "\\s\)") (forward-char) (backward-sexp 1))
        ((looking-back "\\s\(" 1) (backward-char) (forward-sexp 1))))

(defun meow-setup ()
  "General setup function for everything meow."
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
   '("~" . lkn/toggle-case-dwim)
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
   '("G" . end-of-buffer)

   '("gc" . comment-dwim)
   '("gb" . meow-pop-to-mark)
   '("gf" . meow-unpop-to-mark)
   '("gB" . pop-global-mark)
   '("gr" . xref-find-references)
   '("gR" . xref-find-references-and-replace)
   '("gd" . xref-find-definitions)
   '("gD" . xref-find-definitions-other-window)

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
   '("m" . expreg-expand)
   '("M" . meow-join)
   '("n" . expreg-contract)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-avy-goto-char)
   '("S" . meow-avy-goto-char-expand)
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

(use-package meow-vterm
  :ensure (:host github :repo "accelbread/meow-vterm")
  :init (meow-vterm-enable))

(use-package meow
  :demand t
  :diminish (meow-normal-mode meow-insert-mode meow-keypad-mode meow-esc-mode meow-motion-mode meow-beacon-mode)
  :hook (git-commit-mode . meow-insert-mode)
  :custom
  (meow-use-clipboard t)
  :config
  (add-to-list 'meow-mode-state-list '(vterm-mode . insert))
  (meow-setup)
  (meow-global-mode 1))

(use-package meow-tree-sitter
  :when (treesit-available-p)
  :after meow
  :config
  (meow-tree-sitter-register-defaults))

(provide 'lkn-meow)
;;; lkn-meow.el ends here
