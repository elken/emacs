;;; lkn-llm.el -- My grandma used to tell me stories about generating code -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright (C) 2022-2025 Ellis Kenyő <emacs@lkn.mozmail.com>

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
;;; Pretty much just config for gptel, but may also include more
;;; packages over time.
;;; Code:

(use-package shell-maker)

(use-package acp
  :ensure (:host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :commands (agent-shell lkn/agent-shell-toggle)
  :bind
  (:map project-prefix-map
        ("l" . lkn/agent-shell-toggle))
  :custom
  (agent-shell-file-completion-enabled t)
  (agent-shell-show-welcome-message nil)
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  :config
;;;###autoload
  (defun lkn/agent-shell-toggle ()
    "Toggle an agent-shell buffer.
Creates or toggles an agent-shell buffer specific to the current perspective and project."
    (interactive)
    (let* ((project (project-current))
           (agent-config (agent-shell-anthropic-make-claude-code-config))
           (buffer-name (format "%s Agent @ %s" (alist-get :buffer-name agent-config) (persp-current-name)))
           (default-directory (if project (project-root project) default-directory))
           (buffer (get-buffer buffer-name)))
      (if-let ((win (and buffer (get-buffer-window buffer))))
          (delete-window win)
        (unless buffer
          (setq buffer (agent-shell--start :config agent-config
                                           :no-focus t
                                           :new-session t))
          (persp-add-buffer buffer)
          (with-current-buffer buffer
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when-let ((window (get-buffer-window (current-buffer))))
                          (ignore-errors (delete-window window))))
                      t t)))
        (display-buffer-in-side-window
         buffer
         '((side . right)
           (slot . 0)
           (window-width . 0.3)
           (preserve-size . (t . nil))
           (dedicated . t)
           (window-parameters . ((no-delete-other-windows . t)
                                 (mode-line-format . none)))))
        (select-window (get-buffer-window buffer))))))

(use-package agent-shell-sidebar
  ;; Currently broken
  :disabled t
  :after agent-shell
  :ensure (:host github :repo "cmacrae/agent-shell-sidebar")
  :custom
  (agent-shell-sidebar-default-config
   (agent-shell-anthropic-make-claude-code-config)))

(use-package eca
  :commands (eca)
  :ensure (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))

(provide 'lkn-llm)
;;; lkn-llm.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
