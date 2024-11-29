;;; lkn-lsp.el -- Eight Megs And Constantly Swapping -*- lexical-binding: t -*-
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
;;; Configuration for eglot and/or lsp-mode depending on which is my
;;; flavour of the month.
;;; Code:

(use-package eglot
  :ensure nil
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)
  :hook
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  (before-save . eglot-maybe-format-buffer)
  :config
  (defun eglot-maybe-format-buffer (&optional arg)
    "Allow universal argument to disable formatting."
    (interactive "P")
    (when (and (null arg)
	       (eglot-current-server)
	       (null apheleia-mode))
      (eglot-format-buffer)))
  
  ;; Solargraph is a big liar and claims it doesn't support formatting when it does
  ;; Force the capabilities to report dynamicRegistration for
  ;; formatting and formatting itself
  (cl-defmethod eglot-client-capabilities :around (server)
    (let ((caps (cl-call-next-method)))
      (when (cl-find "solargraph" (process-command (jsonrpc--process server))
                     :test #'string-match)
	(setf (cl-getf (cl-getf (cl-getf caps :textDocument) :formatting)
                       :dynamicRegistration) t))
      caps))

  ;; Force the dynamicRegistration to succeed (server again claims it
  ;; doesn't support this)
  (cl-defmethod eglot-register-capability :around (server method id &rest params)
    (if (and (string= method "textDocument/formatting")
             (cl-find "solargraph" (process-command (jsonrpc--process server))
                      :test #'string-match))
	t
      (apply #'cl-call-next-method server method id params)))

  ;; And finally tell the capabilities that the server does actually
  ;; support formatting.
  (cl-defmethod eglot--capabilities :around ((server eglot-lsp-server))
    (let ((caps (cl-call-next-method)))
      (when (cl-find "solargraph" (process-command (jsonrpc--process server))
                     :test #'string-match)
	(setf (cl-getf caps :documentFormattingProvider) t))
      caps))

  ;; Have to specifically use stdio because for jsonrpc processes that
  ;; are network types, we can't get the command from it
  ;; (add-to-list 'eglot-server-programs '(ruby-ts-mode . ("solargraph" "stdio")) nil #'car)

  (setq eglot-server-programs 
      (cons '(ruby-ts-mode . ("ruby-lsp"))
            (cl-remove-if (lambda (x) (eq (car-safe x) 'ruby-ts-mode))
                         eglot-server-programs)))

  ;; Enable formatters, just in case
  (setq-default eglot-workspace-configuration
		'((:solargraph . (:formatting t
				  :diagnostics t))
		  (:ruby-lsp . (:formatter (:enable t))))))

(use-package consult-xref-stack
  :ensure
  (:host github :repo "brett-lempereur/consult-xref-stack" :branch "main")
  :bind
  (("C-," . consult-xref-stack-backward)))

(use-package eglot-booster
  :ensure
  (:host github
   :repo "jdtsmith/eglot-booster"
   :post-build
   (unless (executable-find "emacs-lsp-booster")
     (when (executable-find "cargo")
       (shell-command "cargo install --git https://github.com/blahgeek/emacs-lsp-booster" "*emacs-lsp-booster-install*" "*emacs-lsp-booster-install-errors*"))))
  :after eglot
  :config (eglot-booster-mode))

(provide 'lkn-lsp)
;;; lkn-lsp.el ends here
