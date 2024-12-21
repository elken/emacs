;;; lkn-org.el -- ORGaNICE your life -*- lexical-binding: t -*-
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
;;; Org-Mode for many is the clincher for Emacs, so it makes sense
;;; there's a lot of config here.  I would like to improve the loading
;;; here as org-mode takes a big hit when it loads, but I restart so
;;; infrequently it's not a major issue
;;; Code:

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-journal
  :commands org-journal-new-entry
  :hook (org-journal-after-entry-create-hook . (cmd! (kill-buffer-and-window)))
  :custom
  (org-journal-find-file #'find-file-other-window)
  (org-journal-dir "~/Nextcloud/org/journal")
  :config
  (with-eval-after-load 'meow
    (meow-leader-define-key
     '("j" . org-journal-new-entry))))

(use-package visual-fill-column
  :custom
  (visual-fill-column-width 300)
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

(use-package org-capture
  :ensure nil
  :commands org-capture
  :hook (org-capture-mode . hide-mode-line-mode))

(use-package doct
  :after org-capture
  :config
  (defun org-capture-select-template (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
	       '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
	      (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (nerd-icons-octicon "nf-oct-stop" :face 'nerd-icons-red :v-adjust 0.01) "\tAbort")))))))

  (defun org-mks (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
	      (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
		      (allowed-keys '("\C-g"))
		      (tab-alternatives '("\s" "\t" "\r"))
		      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
		      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
			       (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
		      (insert (format "%s   %s\n" (propertize key 'face '(bold nerd-icons-red)) description))
		      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
		  ;; When we call this via our popup script, set the windows up
		  (when (frame-parameter nil 'popup-frame)
		    (delete-other-windows)
		    (hide-mode-line-mode))
                  (let ((pressed (org--mks-read-key allowed-keys prompt nil)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ((equal pressed "ESC") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))

  (defun org-capture/replace-brackets (link)
    (mapconcat
     (lambda (c)
       (pcase (key-description (vector c))
         ("[" "(")
         ("]" ")")
         (_ (key-description (vector c)))))
     link))

  (defun doct-icon-declaration-to-icon (declaration)
    "Convert :icon declaration to icon"
    (let ((name (pop declaration))
          (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
          (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
          (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
      (apply set `(,name :face ,face :v-adjust ,v-adjust))))

  (declare-function doct-flatten-lists-in "doct" (groups))
  (defun doct-iconify-capture-templates (groups)
    "Add declaration's :icon to each template group in GROUPS."
    (let ((templates (doct-flatten-lists-in groups)))
      (setq doct-templates (mapcar (lambda (template)
                                     (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                 (spec (plist-get (plist-get props :doct) :icon)))
				       (setf (nth 1 template) (concat (doct-icon-declaration-to-icon spec)
								      "\t"
								      (nth 1 template))))
                                     template)
                                   templates))))

  (setq doct-after-conversion-functions '(doct-iconify-capture-templates))

  (setq org-capture-templates
        (doct `(("Home" :keys "h"
                 :icon ("nf-oct-home" :set "octicon" :color "cyan")
                 :file "Home.org"
                 :prepend t
                 :headline "Inbox"
                 :template ("* TODO %?"
                            "%i %a"))
                ("Work" :keys "w"
                 :icon ("nf-fa-building" :set "faicon" :color "yellow")
                 :file "Work.org"
                 :prepend t
                 :headline "Inbox"
                 :template ("* TODO %?"
                            "SCHEDULED: %^{Schedule:}t"
                            "DEADLINE: %^{Deadline:}t"
                            "%i %a"))
                ("Note" :keys "n"
                 :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                 :file "Notes.org"
                 :template ("* %?"
                            "%i %a"))
                ("Journal" :keys "j"
                 :icon ("nf-fa-calendar" :set "faicon" :color "pink")
                 :type plain
                 :function (lambda ()
                             (org-journal-new-entry t)
                             (unless (eq org-journal-file-type 'daily)
			       (org-narrow-to-subtree))
                             (goto-char (point-max)))
                 :template "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                 :jump-to-captured t
                 :immediate-finish t)
                ("Protocol" :keys "P"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* TODO %^{Title}"
                            "Source: %u"
                            "#+BEGIN_QUOTE"
                            "%i"
                            "#+END_QUOTE"
                            "%?"))
                ("Protocol link" :keys "L"
                 :icon ("nf-fa-link" :set "faicon" :color "blue")
                 :file "Notes.org"
                 :template ("* TODO %?"
                            "[[%:link][%:description]]"
                            "Captured on: %U"))
                ("Project" :keys "p"
                 :icon ("nf-oct-repo" :set "octicon" :color "silver")
                 :prepend t
                 :type entry
                 :headline "Inbox"
                 :template ("* %{keyword} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:keyword "")
                 :children (("Task" :keys "t"
                             :icon ("nf-oct-checklist" :set "octicon" :color "green")
                             :keyword "TODO"
                             :file "todo.org")
                            ("Note" :keys "n"
                             :icon ("nf-fa-sticky_note" :set "faicon" :color "yellow")
                             :keyword "%U"
                             :file "notes.org")))))))

;; We basically modify it to include this[1] patch without using org
;; master as there’s a bug there I don’t have time/patience to debug.
;; 
;; [1] https://git.savannah.gnu.org/cgit/emacs/org-mode.git/commit/?id=2ade16bbc
(use-package org
  :ensure nil
  :custom
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width 600)
  (org-archive-location ".archive/Archive_%s::")
  (org-src-preserve-indentation t)       ;; This should ALWAYS be set
  (org-directory "~/Nextcloud/org")
  (org-agenda-files '("~/Nextcloud/org/Home.org" "~/Nextcloud/org/Work.org" "~/Nextcloud/org/Notes.org"))
  (org-use-property-inheritance t)
  (org-cycle-separator-lines -1)
  (org-ellipsis " ▾")
  (org-startup-folded 'content)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-cycle-emulate-tab nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "INPROG(i)" "PROJ(p)" "STORY(s)" "WAIT(w@/!)" "|" "DONE(d@/!)" "KILL(k@/!)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
   ;; The triggers break down to the following rules:

   ;; - Moving a task to =KILLED= adds a =killed= tag
   ;; - Moving a task to =WAIT= adds a =waiting= tag
   ;; - Moving a task to a done state removes =WAIT= and =HOLD= tags
   ;; - Moving a task to =TODO= removes all tags
   ;; - Moving a task to =NEXT= removes all tags
   ;; - Moving a task to =DONE= removes all tags
  (org-todo-state-tags-triggers
   '(("KILL" ("killed" . t))
     ("HOLD" ("hold" . t))
     ("WAIT" ("waiting" . t))
     (done ("waiting") ("hold"))
     ("TODO" ("waiting") ("cancelled") ("hold"))
     ("NEXT" ("waiting") ("cancelled") ("hold"))
     ("DONE" ("waiting") ("cancelled") ("hold"))))

  ;; This settings allows to fixup the state of a todo item without
  ;; triggering notes or log.
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  :init
  (defun lkn/org-remove-kill-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-cut-subtree)
       (pop kill-ring)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/KILL" 'file))

  ;; (keymap-set 'org-mode-map (kbd "C-c DEL k") #'lkn/org-remove-kill-tasks)

  (setq enable-dir-local-variables t)
  (defun elken/find-time-property (property)
    "Find the PROPETY in the current buffer."
    (save-excursion
      (goto-char (point-min))
      (let ((first-heading
             (save-excursion
               (re-search-forward org-outline-regexp-bol nil t))))
	(when (re-search-forward (format "^#\\+%s:" property) nil t)
          (point)))))

  (defun lkn/has-time-property-p (property)
    "Gets the position of PROPETY if it exists, nil if not and empty string if it's undefined."
    (when-let ((pos (elken/find-time-property property)))
      (save-excursion
	(goto-char pos)
	(if (and (looking-at-p " ")
		 (progn (forward-char)
			(org-at-timestamp-p 'lax)))
            pos
          ""))))

  (defun lkn/set-time-property (property &optional pos)
    "Set the PROPERTY in the current buffer.
Can pass the position as POS if already computed."
    (when-let ((pos (or pos (elken/find-time-property property))))
      (save-excursion
	(goto-char pos)
	(if (looking-at-p " ")
            (forward-char)
          (insert " "))
	(delete-region (point) (line-end-position))
	(let* ((now (format-time-string "<%Y-%m-%d %H:%M>")))
          (insert now)))))

  (defun lkn/org-mode-update-properties ()
    (when (derived-mode-p 'org-mode)
      (lkn/set-time-property "LAST_MODIFIED")
      (lkn/set-time-property "DATE_UPDATED")))

  (add-hook 'before-save-hook #'lkn/org-mode-update-properties)
  :config
  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:" (org-id-get-with-outline-path-completion)))


  (org-link-set-parameters "id" :complete 'org-id-complete-link)
  (defun org-fold-core-region (from to flag &optional spec-or-alias)
    "Hide or show lines from FROM to TO, according to FLAG.
SPEC-OR-ALIAS is the folding spec or foldable element, as a symbol.
If SPEC-OR-ALIAS is omitted and FLAG is nil, unfold everything in the region."
    (let ((spec (org-fold-core-get-folding-spec-from-alias spec-or-alias)))
      (when spec (org-fold-core--check-spec spec))
      (with-silent-modifications
	(org-with-wide-buffer
	 ;; Arrange face property of newlines after all the folds
	 ;; between FROM and TO to match the first character before the
	 ;; fold; not the last as per Emacs defaults.  This makes
	 ;; :extend faces span past the ellipsis.
	 ;; See bug#65896.
	 (if flag ; folding
             (when (equal ?\n (char-after to))
               (put-text-property to (1+ to) 'face (get-text-property from 'face)))
           ;; unfolding
           (dolist (region (org-fold-core-get-regions :from from :to to :specs spec))
             (when (equal ?\n (char-after (cadr region)))
               (font-lock-flush (cadr region) (1+ (cadr region))))))
	 (when (eq org-fold-core-style 'overlays)
           (if org-fold-core--keep-overlays
               (mapc
		(lambda (ov)
                  (when (or (not spec)
                            (eq spec (overlay-get ov 'invisible)))
                    (when (and org-fold-core--isearch-active
                               (overlay-get ov 'invisible)
                               (org-fold-core-get-folding-spec-property
				(overlay-get ov 'invisible) :isearch-open))
                      (when (overlay-get ov 'invisible)
			(overlay-put ov 'org-invisible (overlay-get ov 'invisible)))
                      (overlay-put ov 'invisible nil)
                      (when org-fold-core--isearch-active
			(cl-pushnew ov org-fold-core--isearch-overlays)))))
		(overlays-in from to))
             (remove-overlays from to 'org-invisible spec)
             (remove-overlays from to 'invisible spec)))
	 (if flag
	     (if (not spec)
		 (error "Calling `org-fold-core-region' with missing SPEC")
               (if (eq org-fold-core-style 'overlays)
                   ;; Use `front-advance' since text right before to the beginning of
                   ;; the overlay belongs to the visible line than to the contents.
                   (let ((o (make-overlay from to nil
                                          (org-fold-core-get-folding-spec-property spec :front-sticky)
                                          (org-fold-core-get-folding-spec-property spec :rear-sticky))))
                     (when org-fold-core--isearch-active
                       (push o org-fold-core--isearch-overlays))
                     (overlay-put o 'evaporate t)
                     (overlay-put o (org-fold-core--property-symbol-get-create spec) spec)
                     (overlay-put o 'invisible spec)
                     ;; Preserve priority.
                     (overlay-put o 'priority (length (member spec (org-fold-core-folding-spec-list))))
                     (overlay-put o 'isearch-open-invisible #'org-fold-core--isearch-show)
                     (overlay-put o 'isearch-open-invisible-temporary #'org-fold-core--isearch-show-temporary))
		 (put-text-property from to (org-fold-core--property-symbol-get-create spec) spec)
		 (put-text-property from to 'isearch-open-invisible #'org-fold-core--isearch-show)
		 (put-text-property from to 'isearch-open-invisible-temporary #'org-fold-core--isearch-show-temporary)
		 (when (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
                   ;; If the SPEC has highest priority, assign it directly
                   ;; to 'invisible property as well.  This is done to speed
                   ;; up Emacs redisplay on huge (Mbs) folded regions where
                   ;; we don't even want Emacs to spend time cycling over
                   ;; `char-property-alias-alist'.
                   (when (eq spec (caar org-fold-core--specs)) (put-text-property from to 'invisible spec)))))
           (if (not spec)
               (mapc (lambda (spec) (org-fold-core-region from to nil spec)) (org-fold-core-folding-spec-list))
             (when (and (memql 'grab-invisible org-fold-core--optimise-for-huge-buffers)
			(eq org-fold-core-style 'text-properties))
               (when (eq spec (caar org-fold-core--specs))
		 (let ((pos from))
                   (while (< pos to)
                     (if (eq spec (get-text-property pos 'invisible))
			 (let ((next (org-fold-core-next-folding-state-change spec pos to)))
                           (remove-text-properties pos next '(invisible t))
                           (setq pos next))
                       (setq pos (next-single-char-property-change pos 'invisible nil to)))))))
             (when (eq org-fold-core-style 'text-properties)
	       (remove-text-properties from to (list (org-fold-core--property-symbol-get-create spec) nil))))))))))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autoemphasis t)
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t))

;; (use-package org-roam
;;   :custom
;;   (org-roam-directory (expand-file-name "roam" org-directory))
;;   (org-roam-capture-templates
;;    `(("d" "default" plain
;;       (file ,(expand-file-name "templates/roam-default.org" user-emacs-directory))
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
;;       :unnarrowed t))))

(use-package org-protocol
  :ensure nil
  :config
  (defun popup-frame-delete (&rest _)
    "Kill selected frame if it has parameter `popup-frame'."
    (when-let ((bundle (frame-parameter nil 'bundle-identifier)))
      (ns-do-applescript (format "tell application id \"%s\" to activate" bundle)))
    (when (frame-parameter nil 'popup-frame)
      (delete-frame)))

  (defmacro popup-frame-define (command)
    "Define interactive function to call COMMAND in frame with TITLE."
    `(defun ,(intern (format "popup-frame-%s" command)) ()
       (interactive)
       (let* ((display-buffer-alist '(("")
				      (display-buffer-full-frame)))
	      (bundle-identifier (when IS-MAC
				   (ns-do-applescript "tell application \"System Events\" to get bundle identifier of first process whose frontmost is true")))
	      (frame (make-frame
		      `((title . ,(format "popup-frame-%s" ',command))
			(window-system . ns)
			(menu-bar-lines . 1)
			(transient . t)
			(height . 25)
			(width . 70)
			(popup-frame . t)
			(bundle-identifier . ,bundle-identifier)))))
	 (select-frame-set-input-focus frame)
	 (switch-to-buffer " popup-frame-hidden-buffer")
	 (condition-case nil
             (progn
	       (call-interactively ',command)
	       (delete-other-windows)
	       (hide-mode-line-mode))
	   ((quit error user-error)
	    (progn
	      (when bundle-identifier
		(ns-do-applescript (format "tell application id \"%s\" to activate" bundle-identifier)))
	      (delete-frame frame)))))))
  
  (popup-frame-define org-capture)
  (add-hook 'org-capture-after-finalize-hook #'popup-frame-delete))

(use-package org-tempo
  :ensure nil
  :after org
  :init
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("els" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package toc-org
  :hook
  (org-mode . toc-org-enable)
  :custom
  (toc-org-hrefify-default "gh")
  (toc-org-max-depth 5)
  :config
  (defun lkn/toc-org-inhibit-scrolling-a (fn &rest args)
    "Prevent the jarring scrolling that occurs when ToC is regenerated."
    (let ((p (set-marker (make-marker) (point)))
	  (s (window-start)))
      (prog1 (apply fn args)
	(goto-char p)
	(set-window-start nil s t)
	(set-marker p nil))))

  (advice-add 'toc-org-insert-toc :around #'lkn/toc-org-inhibit-scrolling-a))

(provide 'lkn-org)
;;; lkn-org.el ends here
