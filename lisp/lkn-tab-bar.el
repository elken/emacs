;;; lkn-tab-bar.el -- Tabs are overrated really -*- lexical-binding: t -*-
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
  `((t
     :foreground ,(face-attribute 'default :foreground)
     :background ,(face-attribute 'mode-line-inactive :background)
     :box (:line-width 4 :color ,(face-attribute 'mode-line-inactive :background))))
  "Face for a workspace tab."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-selected-workspace-tab
  `((t
     :inherit lkn-tab-bar-workspace-tab
     :weight bold
     :foreground ,(face-attribute 'highlight :foreground)
     :background ,(face-attribute 'highlight :background)
     :box (:line-width 4 :color ,(face-attribute 'highlight :background))))
  "Face for a selected workspace tab."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-now-playing-container
  `((t
     :background ,(face-attribute 'mode-line-inactive :background)
     :box (:line-width 4 :color ,(face-attribute 'mode-line-inactive :background))))
  "Face for the now-playing container background."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-now-playing-text
  `((t
     :inherit lkn-tab-bar-now-playing-container
     :font ,(face-attribute 'variable-pitch :font)
     :weight bold
     :foreground ,(face-attribute 'default :foreground)))
  "Face for the now-playing text."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-now-playing-player-icon
  `((t
     :inherit lkn-tab-bar-now-playing-container
     :height 1.5
     :foreground ,(face-attribute 'success :foreground)))
  "Face for the now-playing player icon."
  :group 'lkn-tab-bar-faces)

(defface lkn-tab-bar-now-playing-icon
  `((t
     :inherit lkn-tab-bar-now-playing-container
     :height 1.5
     :foreground ,(face-attribute 'default :foreground)))
  "Face for the now-playing icon."
  :group 'lkn-tab-bar-faces)

(defun lkn-tab-bar--workspace-tab (name index is-current)
  "Create a tab item for workspace NAME at INDEX.
IS-CURRENT indicates if this is the current workspace."
  (let* ((face (if is-current
                   'lkn-tab-bar-selected-workspace-tab
                 'lkn-tab-bar-workspace-tab))
         (close-button (propertize " × "
                                   'face `(:inherit ,face :weight bold)
                                   'close-tab t
                                   'help-echo "Close workspace"))
         (separator (propertize " " 'face 'tab-bar))
         (label (concat
                 separator
                 (propertize (format " %d" (1+ index))
                             'face `(:inherit ,face :weight bold))
                 (propertize (format " %s" name)
                             'face `,face)
                 close-button
                 separator)))
    `(,(intern (format "workspace-%s" name))
      menu-item
      ,label
      ,(lambda (event)
         (interactive "e")
         (let ((target (posn-string (event-start event))))
           (if (and target (get-text-property (cdr target) 'close-tab (car target)))
               ;; Close button clicked
               (when (yes-or-no-p (format "Close workspace '%s'? " name))
                 (persp-kill name))
             ;; Tab clicked - switch to workspace
             (unless is-current
               (persp-switch name))))))))

(defun lkn-tab-bar--workspaces ()
  "Return a list of workspace tab items for the tab-bar."
  (let ((persps (persp-names))
        (current-persp (persp-current-name)))
    (when (< 1 (length persps))
      (cl-loop for persp in persps
               for index from 0
               collect (lkn-tab-bar--workspace-tab
                        persp
                        index
                        (equal persp current-persp))))))

(defun lkn-tab-bar--now-playing ()
  "Return the current now-playing status."
  (when (eieio-object-p doom-modeline-now-playing-current-provider)
    (let ((padding-left (propertize " " 'face 'lkn-tab-bar-now-playing-container))
          (padding-right (propertize " " 'face 'lkn-tab-bar-now-playing-container))
          (player-icon (propertize
                        (substring-no-properties (doom-modeline-now-playing--icon))
                        'face 'lkn-tab-bar-now-playing-player-icon))
          (playing-icon (propertize
                         (concat " "
                                 (substring-no-properties (doom-modeline-now-playing--playing))
                                 " ")
                         'face 'lkn-tab-bar-now-playing-icon))
          (text (propertize
                 (substring-no-properties (doom-modeline-now-playing--text))
                 'face 'lkn-tab-bar-now-playing-text)))
      (concat padding-left
              player-icon
              playing-icon
              text
              padding-right))))

(defun lkn-tab-bar-workspaces-format ()
  "Format workspaces for tab-bar."
  (lkn-tab-bar--workspaces))

(defun lkn-tab-bar-now-playing-format ()
  "Format now-playing for tab-bar."
  (when-let* ((text (lkn-tab-bar--now-playing)))
    `((now-playing menu-item ,text ignore))))

(defun lkn-tab-bar-format-align-center ()
  "Align tab-bar items to the center.
This calculates the necessary spacing to center the workspaces."
  (let* ((rest (cdr (memq 'lkn-tab-bar-format-align-center tab-bar-format)))
         ;; Get items to center (until next alignment marker)
         (items-to-center (seq-take-while
                           (lambda (item)
                             (not (memq item '(tab-bar-format-align-right
                                               lkn-tab-bar-format-align-center))))
                           rest))
         ;; Calculate the formatted string
         (center-str (format-mode-line
                      (mapcan (lambda (item)
                                (when (functionp item)
                                  (funcall item)))
                              items-to-center)))
         ;; Get character width (simpler and more reliable for tab-bar)
         (center-chars (length center-str)))
    `((align-center menu-item
                    ,(propertize " " 'display
                                 ;; Align to center, then back up by half the content width
                                 `(space :align-to (- center ,(/ center-chars 2))))
                    ignore))))

(provide 'lkn-tab-bar)
;;; lkn-tab-bar.el ends here
