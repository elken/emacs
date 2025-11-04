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
  :commands (agent-shell agent-shell-sidebar-toggle)
  :hook (agent-shell-mode . hide-mode-line-mode)
  :custom
  (agent-shell-file-completion-enabled t)
  (agent-shell-show-welcome-message nil)
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)))

(use-package agent-shell-sidebar
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
