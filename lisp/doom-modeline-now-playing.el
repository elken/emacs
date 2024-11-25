;;; doom-modeline-now-playing.el --- Segment for Doom Modeline to show media player information -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2024 Ellis Kenyő
;;
;; Author: Ellis Kenyő <me@elken.dev>
;; Maintainer: Ellis Kenyő <me@elken.dev>
;; Created: January 23, 2021
;; Modified: November 22, 2024
;; Version: 0.2.0
;; Homepage: https://github.com/elken/doom-modeline-now-playing
;; Package-Requires: ((emacs "24.4") (doom-modeline "3.0.0") (async "1.9.3"))
;; SPDX-License-Identifier: GPL3
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Media player support for doom-modeline.
;;  Can be extended with custom providers.
;;
;;  To enable:
;;  (doom-modeline-now-playing-timer)
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline)
(require 'async)
(require 'eieio)

;;
;; Custom variables
;;

(defgroup doom-modeline-now-playing nil
  "Settings related to doom-modeline-now-playing"
  :group 'doom-modeline)

(defcustom doom-modeline-now-playing t
  "Whether to display the now-playing segment."
  :type 'boolean
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-interval 5
  "Default delay in seconds to update the status."
  :type 'integer
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-max-length 40
  "Maximum length of output. Truncates with `...' after."
  :type 'integer
  :group 'doom-modeline-now-playing)

(defcustom doom-modeline-now-playing-current-provider
  (cond
   ((eq system-type 'gnu/linux)
    (doom-modeline-now-playing-playerctl))
   ((eq system-type 'darwin)
    (doom-modeline-now-playing-osascript)))
  "Current media player provider.
Defaults to playerctl on linux and osascript on macOS."
  :type '(choice (const :tag "None" nil)
	  (object :tag "Provider"))
  :group 'doom-modeline-now-playing)

(defface doom-modeline-now-playing-icon
  '((t (:inherit success)))
  "Face for the now-playing icon"
  :group 'doom-modeline-faces)

(defface doom-modeline-now-playing-text
  '((t (:inherit bold)))
  "Face for the now-playing text"
  :group 'doom-modeline-faces)

;;
;; Provider Interface
;;
(defclass doom-modeline-now-playing-provider ()
  ((name :initarg :name
         :type string
         :documentation "Name of the provider")
   (supported-p :initarg :supported-p
		:documentation "Bool or predicate to determine if the provider should run on the current setup.")
   (current-info :initform nil
		 :documentation "Current player status or nil if not set"))
  "Base class for media player providers."
  :abstract t)

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-provider))
  "Get current playback information from PROVIDER.
Should return (player-name status track-info) or nil if no player is active."
  (error "Method must be implemented by subclass"))

(cl-defmethod doom-modeline-now-playing-provider-play-pause ((provider doom-modeline-now-playing-provider) player)
  "Toggle playback for PLAYER using PROVIDER."
  (error "Method must be implemented by subclass"))

(cl-defmethod doom-modeline-now-playing-provider-update ((provider doom-modeline-now-playing-provider))
  "Update the current info for PROVIDER."
  (when (let ((supported (oref provider supported-p)))
          (if (functionp supported)
              (funcall supported)
            supported))
    (oset provider current-info (doom-modeline-now-playing-provider-get-info provider))))

(defclass doom-modeline-now-playing-osascript (doom-modeline-now-playing-provider)
  ((name :initform "osascript")
   (supported-p :initform (lambda ()
                           (and (eq system-type 'darwin)
                                (executable-find "osascript")))))
  "macOS AppleScript provider implementation.")

(cl-defmethod doom-modeline-now-playing-provider-get-info ((provider doom-modeline-now-playing-osascript))
  (cl-loop for player in '("Spotify" "Music")
           for script = (format "
tell application \"%s\"
    if running then
        set playerState to (player state as string)
        set currentArtist to (artist of current track as string)
        set currentTrack to (name of current track as string)
        return \"%s|\" & playerState & \"|\" & currentArtist & \" - \" & currentTrack
    end if
end tell" player (downcase player))
           for result = (condition-case nil
                          (string-trim
                           (shell-command-to-string
                            (format "osascript -e '%s'" script)))
                        (error nil))
           when (and result (not (string-empty-p result)))
           return (pcase-let ((`(,player ,status ,text)
                              (split-string result "|")))
                   (now-playing-status-create
                    :player player
                    :status status 
                    :text text))))

;;
;; Core functionality
;;

(cl-defstruct (now-playing-status (:constructor now-playing-status-create))
  "Structure holding the current playback status."
  status player text)

(defun doom-modeline-now-playing--update ()
 "Update the current playback status."
 (when (and doom-modeline-now-playing
            (> doom-modeline-now-playing-interval 0)
            doom-modeline-now-playing-provider)
   (let ((supported (oref doom-modeline-now-playing-provider supported-p)))
     (when (if (functionp supported)
               (funcall supported)
             supported)
       (async-start
        `(lambda ()
           (require 'subr-x)
           ,(async-inject-variables "\\`\\(load-path\\|auth-sources\\|system-type\\)\\'")
           (let ((get-info-fn ',(doom-modeline-now-playing-provider-get-info 
                                doom-modeline-now-playing-provider)))
             (when-let ((info (funcall get-info-fn)))
               (oset doom-modeline-now-playing-provider current-info info)))))))))

(defun doom-modeline-now-playing-toggle-status ()
 "Toggle play/pause for the current player."
 (interactive)
 (when-let* ((provider doom-modeline-now-playing-provider)
             (status (oref provider current-info))
             (player (now-playing-status-player status)))
   (doom-modeline-now-playing-provider-play-pause provider player)
   (doom-modeline-now-playing--update)))



;;
;; Timer management
;;

(defvar doom-modeline-now-playing--timer nil
  "Timer for updating the playback status.")

(defun doom-modeline-now-playing-timer ()
  "Start/Stop the timer for now-playing update."
  (when (timerp doom-modeline-now-playing--timer)
    (cancel-timer doom-modeline-now-playing--timer))
  (setq doom-modeline-now-playing--timer
        (when doom-modeline-now-playing
          (run-with-timer 1 doom-modeline-now-playing-interval
                         #'doom-modeline-now-playing--update))))

(doom-modeline-add-variable-watcher
 'doom-modeline-now-playing
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-now-playing val)
     (doom-modeline-now-playing-timer))))

;;
;; Modeline segment
;;

(doom-modeline-def-segment now-playing
  "Display current media playback status."
  (when (and doom-modeline-now-playing
             (doom-modeline--active)
             doom-modeline-now-playing-status)
    (let ((player (now-playing-status-player doom-modeline-now-playing-status))
          (status (now-playing-status-status doom-modeline-now-playing-status))
          (text   (now-playing-status-text   doom-modeline-now-playing-status)))
      (concat
       (doom-modeline-spc)
       (if (string= "spotify" player)
           (doom-modeline-icon 'faicon "nf-fa-spotify" "" "#"
                              :face 'doom-modeline-now-playing-icon
                              :v-adjust -0.0575)
         (doom-modeline-icon 'faicon "nf-fa-music" "" "#"
                            :face 'doom-modeline-now-playing-icon
                            :v-adjust -0.0575))
       (doom-modeline-spc)
       (propertize (if (equal status "playing")
                      (doom-modeline-icon 'faicon "nf-fa-play" "" ">"
                                        :v-adjust -0.0575)
                    (doom-modeline-icon 'faicon "nf-fa-pause" "" "||"
                                      :v-adjust -0.0575))
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: Toggle player status"
                  'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1]
                                       'doom-modeline-now-playing-toggle-status)
                             map))
       (doom-modeline-spc)
       (propertize
        (truncate-string-to-width text doom-modeline-now-playing-max-length nil nil "...")
        'face 'doom-modeline-now-playing-text)))))

(provide 'doom-modeline-now-playing)
;;; doom-modeline-now-playing.el ends here
