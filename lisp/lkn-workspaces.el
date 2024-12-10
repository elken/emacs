;;; lkn-workspaces.el -- Mangaging loads of frames and buffers sucks -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2024Ellis Kenyő

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
;;; I work a lot with a couple of projects at once, and managing all
;;; these buffers at once isn't fun.
;;; Code:

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
        (persp-rename (project-name (project-current)))
	(setq-local default-directory (project-root (project-current)))))))

(provide 'lkn-workspaces)
;;; lkn-workspaces.el ends here
