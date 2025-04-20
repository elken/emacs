;;; lkn-workspaces.el -- Mangaging loads of frames and buffers sucks -*- lexical-binding: t -*-
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
;;; I work a lot with a couple of projects at once, and managing all
;;; these buffers at once isn't fun.
;;; Code:

(use-package perspective
  :init (persp-mode)
  :hook (kill-emacs . persp-state-save)
  :bind
  (("C-x C-b" . persp-switch-to-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c TAB"))
  (persp-show-modestring nil)
  (persp-state-default-file (lkn/cache-dir "perspectives"))
  :config
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
        (setq-local default-directory (project-root (project-current))))))

  ;; Handle vterm buffer scrollback

  ;; First we adjust what we count as an interesting buffer
  (defun persp--state-interesting-buffer-p (buffer)
    (and (buffer-name buffer)
         (or
          (string-match "vterm\\*" (buffer-name buffer))
          (not (string-match "^[[:space:]]*\\*" (buffer-name buffer))))
         (or (buffer-file-name buffer)
             (with-current-buffer buffer (memq major-mode '(dired-mode vterm-mode))))))



  ;; We defined a custom variable to handle how many lines of our
  ;; prompt to trim off (to prevent us just constantly having blank
  ;; prompts on unused terminals)
  (defcustom persp-vterm-prompt-length 2
    "How many lines to trim off when you persist a vterm buffer."
    :type 'natnum
    :group 'perspective-mode)

  ;; Then we update how we want to handle storing the buffer data. For
  ;; now we just add in all the scrollback, but we may look into
  ;; truncation.
  (defun persp--state-file-data ()
    (cl-loop for buffer in (buffer-list)
             if (persp--state-interesting-buffer-p buffer)
             collect (or (buffer-file-name buffer)
                         (with-current-buffer buffer ; dired special case
                           (cond
                            ((eq major-mode 'dired-mode) default-directory)
                            ((eq major-mode 'vterm-mode)
                             (progn
                               (require 'vterm)
                               (goto-char (point-max))
                               (vterm-previous-prompt 1)
                               (previous-line persp-vterm-prompt-length)
                               (end-of-line)
                               `(,(buffer-name) ,default-directory ,(buffer-substring (point-min) (point))))))))))

  ;; Last we have to update the loader to handle how we want to create
  ;; the new buffer.

  ;; All we do is create a new buffer essentially with all the
  ;; previous scrollback. This has an unfortunate issue currently
  ;; where it inserts an ugly empty prompt, but short of doing some
  ;; hacks to understand what the prompt is I'll leave this for the
  ;; time being.
  (defun persp-state-load (file)
    "Restore the perspective state saved in FILE.

FILE defaults to the value of persp-state-default-file if it is
set.

Frames are restored, along with each frame's perspective list and merge list.
Each perspective's buffer list and window layout are also
restored."
    (interactive (list
                  (read-file-name "Restore perspective state from file: "
                                  persp-state-default-file
                                  persp-state-default-file)))
    (unless (file-exists-p file)
      (user-error "File not found: %s" file))
    (persp-mode 1)
    ;; before hook
    (run-hooks 'persp-state-before-load-hook)
    ;; actually load
    (let ((tmp-persp-name (format "%04x%04x" (random (expt 16 4)) (random (expt 16 4))))
          (frame-count 0)
          (state-complete (persp--state-complete-v2
                           (read
                            (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string))))))
      ;; open all files in a temporary perspective to avoid polluting "main"
      (persp-switch tmp-persp-name)
      (cl-loop for file in (persp--state-complete-files state-complete) do
               (cond
                ((stringp file)
                 (when (file-exists-p file)
                   (find-file file)))
                ((listp file)
                 (cl-destructuring-bind (name dir contents) file
                   (let ((default-directory dir))
                     (require 'vterm)
                     (with-current-buffer (get-buffer-create name)
                       (insert contents)
                       (vterm-mode)))))))
      ;; iterate over the frames
      (cl-loop for frame in (persp--state-complete-frames state-complete) do
               (cl-incf frame-count)
               (let ((emacs-frame (if (> frame-count 1) (make-frame-command) (selected-frame)))
                     (frame-persp-table (persp--state-frame-v2-persps frame))
                     (frame-persp-order (reverse (persp--state-frame-v2-order frame)))
                     (frame-persp-merge-list (persp--state-frame-v2-merge-list frame)))
                 (with-selected-frame emacs-frame
                   ;; restore the merge list
                   (set-frame-parameter emacs-frame 'persp-merge-list frame-persp-merge-list)
                   ;; iterate over the perspectives in the frame in the appropriate order
                   (cl-loop for persp in frame-persp-order do
                            (let ((state-single (gethash persp frame-persp-table)))
                              (persp-switch persp)
                              (set-frame-parameter nil 'persp-merge-list frame-persp-merge-list)
                              (cl-loop for buffer in (persp--state-single-buffers state-single) do
                                       (persp-add-buffer buffer))
                              ;; XXX: split-window-horizontally is necessary for
                              ;; window-state-put to succeed? Something goes haywire with root
                              ;; windows without it.
                              (split-window-horizontally)
                              (window-state-put (persp--state-single-windows state-single)
                                                (frame-root-window emacs-frame)
                                                'safe))))))
      ;; cleanup
      (persp-kill tmp-persp-name))
    ;; after hook
    (run-hooks 'persp-state-after-load-hook)))

(provide 'lkn-workspaces)
;;; lkn-workspaces.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
