fundamental-mode

(today (format-time-string "%Y-%m-%d"))
(me "Ellis Kenyő")
(email "emacs@lkn.mozmail.com")

lisp-mode emacs-lisp-mode

(lambda "(lambda (" p ")" n> r> ")")

emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let* (" p ")" n> r> ")")
(whenlet "(when-let* (" p ")" n> r> ")")
(whilelet "(while-let (" p ")" n> r> ")")
(andlet "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(lett "(let* (" p ")" n> r> ")")
(pcaselet "(pcase-let (" p ")" n> r> ")")
(pcaselett "(pcase-let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
        " -- " p " -*- lexical-binding: t -*-" n
        ";; Sources are available from https://github.com/elken/emacs" n n
        ";; Copyright (C) 2022-" (format-time-string "%Y") " " (i me) " <" (i email) ">" n n
        ";; This file is free software; you can redistribute it and/or modify" n
        ";; it under the terms of the GNU General Public License as published by" n
        ";; the Free Software Foundation; either version 3, or (at your option)" n
        ";; any later version." n n
        ";; This file is distributed in the hope that it will be useful," n
        ";; but WITHOUT ANY WARRANTY; without even the implied warranty of" n
        ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" n
        ";; GNU General Public License for more details." n n
        ";; For a full copy of the GNU General Public License" n
        ";; see <https://www.gnu.org/licenses/>." n n
        ";;; Commentary:" n
        ";;; " p n>
        ";;; Code:" n n n)
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
         " ends here" n)
(default-locals
 ";; Local Variables:" n
 ";; byte-compile-warnings: (not free-vars unresolved)" n
 ";; no-byte-compile: t" n
 ";; no-native-compile: t" n
 ";; no-update-autoloads: t" n
 ";; End:")
(package (i header) r n n (i provide) (i default-locals))
(usep "(use-package " p n> r>
      ")")

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
