;;; lkn-tab-bar.el -- Tabs are overrated really -*- lexical-binding: t -*-
;; Sources are available from https://github.com/elken/emacs

;; Copyright ©️ 2022-2024 Ellis Kenyő

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
;;; Hijacking the global-tab-bar to display more useful info
;;; Code:

(declare-function "persp-names" 'perspective)

(defgroup lkn-tab-bar nil
  "Options related to my custom tab-bar."
  :group 'tab-bar)

(defgroup lkn-tab-bar-faces nil
  "Faces for the tab-bar."
  :group 'lkn-tab-bar)

(defface lkn-tab-bar-workspace-tab
  `((t :inherit doom-modeline
       :foreground ,(face-attribute 'default :foreground)))
  "Face for a workspace tab."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-selected-workspace-tab
  '((t :inherit (cursor)))
  "Face for a selected workspace tab."
  :group 'lkn-tab-bar-faces)

(defun lkn-tab-bar--workspaces ()
  "Return a list of the current workspaces."
  (nreverse
   (let ((persps (persp-names))
         (persp (persp-current-name)))
     (when (< 1 (length persps))
       (seq-reduce
        (lambda (acc elm)
          (let ((face (if (equal persp elm)
                          'lkn-tab-bar-selected-workspace-tab
                        'lkn-tab-bar-workspace-tab)))
            (push
	     (concat
              (propertize (format " %d" (1+ (cl-position elm persps)))
                          'face
                          `(:inherit ,face
                            :weight bold))
              (propertize (format " %s " elm)
                          'face `,face))
	     acc)))
        persps
        `(,(propertize (persp-current-name) 'invisible t)))))))

(provide 'lkn-tab-bar)
;;; lkn-tab-bar.el ends here
