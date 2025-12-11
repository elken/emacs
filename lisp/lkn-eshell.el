;;; lkn-eshell.el -- Finally a decent shell -*- lexical-binding: t -*-
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
;;; Config and related packages for eshell.  Over time I would like
;;; eshell to replace all my shell usage
;;; Code:

(use-feature eshell
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all)
  (eshell-term-name "xterm-256color")
  :hook
  (eshell-mode . (lambda ()
                   (display-line-numbers-mode -1)
                   (completion-preview-mode -1)))
  :config
  ;; We use popper for this but for whatever reason it doesn't work
  ;; the first time it's opened
  (add-to-list 'display-buffer-alist
               '("\\*.*eshell.*\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.15)))

  (defun eshell-fn-on-files (fun1 fun2 args)
    "Call FUN1 on the first element in list, ARGS.
Call FUN2 on all the rest of the elements in ARGS."
    (unless (null args)
      (let ((filenames (flatten-list args)))
        (funcall fun1 (car filenames))
        (when (cdr filenames)
          (mapcar fun2 (cdr filenames))))
      ;; Return an empty string, as the return value from `fun1'
      ;; probably isn't helpful to display in the `eshell' window.
      ""))

;;;###autoload
  (defun eshell/less (&rest files)
    "Essentially an alias to the `view-file' function."
    (eshell-fn-on-files 'view-file 'view-file-other-window files))
  (defalias 'eshell/more 'eshell/less)

;;;###autoload
  (defun eshell/mkdcd (dir)
    "Create a directory (DIR) then cd into it."
    (make-directory dir t)
    (eshell/cd dir))

;;;###autoload
  (defun eshell/rinku (&rest args)
    "Fetch link preview with rinku and display image inline.

Usage: rinku https://soundcloud.com/shehackedyou
       rinku --render https://soundcloud.com/shehackedyou"
    (unless args
      (error "rinku: No arguments provided"))
    (let* ((output (with-temp-buffer
                     (apply #'call-process "rinku" nil t nil (seq-uniq (cons "--preview" args)))
                     (buffer-string)))
           (metadata (condition-case nil
                         (json-read-from-string output)
                       (error nil))))
      (if metadata
          (concat
           (if (map-elt metadata 'image)
               (eshell/cat (map-elt metadata 'image))
             "\n")
           (when (map-elt metadata 'title)
             (concat (map-elt metadata 'title)
                     "\n\n")))
        output)))

  (defun eshell-maybe-bol ()
    "Jump to the beginning of the command line, or the real beginning of the
line if already there."
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (if (= p (point))
          (beginning-of-line))))

  (with-eval-after-load 'esh-mode
    (define-key eshell-mode-map (kbd "C-a") 'eshell-bol))

  (defun adviced:eshell/cat (orig-fun &rest args)
  "Like `eshell/cat' but with image support."
  (if (seq-every-p (lambda (arg)
                     (and (stringp arg)
                          (file-exists-p arg)
                          (image-supported-file-p arg)))
                   args)
      (with-temp-buffer
        (insert "\n")
        (dolist (path args)
          (let ((spec (create-image
                       (expand-file-name path)
                       (image-type-from-file-name path)
                       nil :max-width 350
                       :conversion (lambda (data) data))))
            (image-flush spec)
            (insert-image spec))
          (insert "\n"))
        (insert "\n")
        (buffer-string))
    (apply orig-fun args)))

(advice-add #'eshell/cat :around #'adviced:eshell/cat))

(use-feature em-xtra
  :after eshell)

(use-package pcre2el
  :ensure (:host github :repo "joddie/pcre2el")
  :config
  (defmacro prx (&rest expressions)
    "Convert the rx-compatible regular EXPRESSIONS to PCRE.
  Most shell applications accept Perl Compatible Regular Expressions."
    `(rx-let ((integer (1+ digit))
              (float   (seq integer "." integer))
              (b256    (seq (optional (or "1" "2"))
                            (regexp "[0-9]\\{1,2\\}")))
              (ipaddr  (seq b256 "." b256 "." b256 "." b256))
              (ipv6    (seq (1+ (seq (1+ hex) ":")) (1+ hex)))
              (mac-address (seq (= 2 hex) ":" (= 2 hex) ":" (= 2 hex) ":"
                                (= 2 hex) ":" (= 2 hex) ":" (= 2 hex)))
              (hostname (seq (1+ (any alnum "-")) (0+ (seq "." (1+ (any alnum "-"))))))
              (url     (seq (or "http" "https") "://"
                            (1+ (any alnum "-._~:/?#[]@!$&'()*+,;=%"))))
              (time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
              (email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
              (date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
              (ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
              (uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
              (guid    (seq uuid))
              (filepath (seq (optional "/") (1+ (seq (1+ (any alnum "-._")) (optional "/")))))
              (win-path (seq (any alpha) ":\\" (1+ (seq (1+ (any alnum "-._")) (optional "\\")))))
              (hex-color (seq "#" (or (= 3 hex) (= 6 hex) (= 8 hex))))
              (semver  (seq (1+ digit) "." (1+ digit) "." (1+ digit)
                            (optional (seq "-" (1+ (any alnum ".-"))))))
              (git-sha (or (= 40 hex) (= 7 hex)))
              (json-string (seq "\"" (0+ (or (not (any "\"\\"))
                                             (seq "\\" anything))) "\"")))
       (rxt-elisp-to-pcre (rx ,@expressions)))))

(use-package eshell-p10k
  :ensure (:host github :repo "elken/eshell-p10k")
  :custom
  (eshell-prompt-function #'eshell-p10k-default-prompt)
  (eshell-prompt-regexp eshell-p10k-prompt-string))

(use-package eshell-syntax-highlighting
  :after eshell
  :hook
  (eshell-mode . eshell-syntax-highlighting-mode))

(use-package pcmpl-args
  :after pcomplete)

(use-package fish-completion
  :when (executable-find "fish")
  :ensure (:host github :repo "LemonBreezes/emacs-fish-completion")
  :hook
  (eshell-mode . fish-completion-mode))

(provide 'lkn-eshell)
;;; lkn-eshell.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
