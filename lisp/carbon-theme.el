;;; carbon-theme.el --- Carbon theme -*- lexical-binding: t -*-

;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: faces, themes
;; Homepage: https://github.com/elken/emacs

;;; Commentary:
;; A theme based on Carbon Design System colors.

;;
;;; Variables

(deftheme carbon "A theme based on IBM Carbon's design system")

(let* ((class '((class color) (min-colors 89)))
       (black "#000000")
       (blue-40 "#78a9ff")
       (blue-50 "#4589ff")
       (blue-60 "#0f62fe")
       (cyan-40 "#33b1ff")
       (cyan-50 "#1192e8")
       (gray-30 "#c6c6c6")
       (gray-40 "#a8a8a8")
       (gray-50 "#8d8d8d")
       (gray-60 "#6f6f6f")
       (gray-70 "#525252")
       (gray-80 "#393939")
       (gray-90 "#262626")
       (gray-100 "#161616")
       (green-40 "#42be65")
       (magenta-40 "#ff7eb6")
       (magenta-50 "#ee5396")
       (orange-40 "#ff832b")
       (orange-50 "#eb6200")
       (purple-50 "#a56eff")
       (red-50 "#fa4d56")
       (teal-30 "#3ddbd9")
       (teal-40 "#08bdba")
       (white "#f4f4f4")
       (white-100 "#ffffff"))
  
  (custom-theme-set-faces
   'carbon
   `(default ((,class (:background ,black :foreground ,white))))
   
   ;; General
   `(cursor ((,class (:background ,magenta-50 :foreground ,black))))
   `(fringe ((,class (:background ,black :foreground ,gray-70))))
   `(line-number ((,class (:foreground ,gray-50 :background ,black))))
   `(line-number-current-line ((,class (:foreground ,magenta-50 :background ,black :weight bold))))
   `(hl-line ((,class (:background ,gray-90))))
   `(region ((,class (:background ,magenta-50 :foreground ,white))))
   `(minibuffer-prompt ((,class (:foreground ,magenta-50 :weight bold))))

   ;; Tab bar faces
   `(tab-bar ((,class (:background ,black :foreground ,gray-40))))
   `(tab-bar-tab ((,class (:background ,gray-90 :foreground ,white :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,black :foreground ,gray-40))))
   `(tab-bar-tab-group-current ((,class (:background ,gray-90 :foreground ,magenta-50))))
   `(tab-bar-tab-group-inactive ((,class (:background ,black :foreground ,gray-50))))
   `(tab-bar-tab-ungrouped ((,class (:background ,black :foreground ,gray-40))))
   
   ;; Mode line
   `(mode-line ((,class (:background ,gray-100 :foreground ,white))))
   `(mode-line-inactive ((,class (:background ,gray-90 :foreground ,gray-40))))
   `(mode-line-highlight ((,class (:foreground ,magenta-50 :weight bold))))  ; Changed from magenta-40
   `(mode-line-buffer-id ((,class (:foreground ,magenta-40 :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,gray-40))))
   `(doom-modeline-buffer-file ((,class (:foreground ,white :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,red-50 :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,magenta-50 :weight bold))))
   `(doom-modeline-buffer-minor-mode ((,class (:foreground ,gray-50))))
   `(doom-modeline-project-dir ((,class (:foreground ,gray-40))))
   `(doom-modeline-project-root-dir ((,class (:foreground ,gray-40))))
   `(doom-modeline-project-parent-dir ((,class (:foreground ,gray-40))))
   `(doom-modeline-info ((,class (:foreground ,magenta-40))))
   `(doom-modeline-warning ((,class (:foreground ,orange-40))))
   `(doom-modeline-urgent ((,class (:foreground ,red-50 :weight bold))))
   `(doom-modeline-debug ((,class (:foreground ,blue-40))))
   `(doom-modeline-bar ((,class (:background ,magenta-50))))
   `(doom-modeline-bar-inactive ((,class (:background ,gray-90))))

   ;; Search
   `(isearch ((,class (:background ,magenta-50 :foreground ,white))))
   `(lazy-highlight ((,class (:background ,magenta-40 :foreground ,black))))

   ;; Diff
   `(diff-added ((,class (:foreground ,green-40))))
   `(diff-removed ((,class (:foreground ,red-50))))
   `(diff-changed ((,class (:foreground ,magenta-50))))
   `(diff-header ((,class (:background ,gray-90 :foreground ,white))))

   ;; transient
   `(transient-heading ((,class (:foreground ,magenta-50 :weight bold))))
   `(transient-key ((,class (:foreground ,magenta-40))))
   `(transient-argument ((,class (:foreground ,green-40))))
   `(transient-value ((,class (:foreground ,blue-40))))

   ;; Font-lock
   `(font-lock-keyword-face ((,class (:foreground ,blue-40))))
   `(font-lock-builtin-face ((,class (:foreground ,magenta-50))))
   `(font-lock-comment-face ((,class (:foreground ,gray-40 :italic t))))
   `(font-lock-string-face ((,class (:foreground ,purple-50))))
   `(font-lock-function-name-face ((,class (:foreground ,magenta-50 :bold t))))
   `(font-lock-type-face ((,class (:foreground ,teal-30))))
   `(font-lock-constant-face ((,class (:foreground ,blue-50))))
   `(font-lock-variable-name-face ((,class (:foreground ,white))))
   `(font-lock-property-use-face ((,class (:foreground ,blue-40))))
   `(font-lock-doc-face ((,class (:foreground ,purple-50))))
   `(font-lock-escape-face ((,class (:foreground ,teal-30))))
   `(font-lock-number-face ((,class (:foreground ,blue-50))))
   `(font-lock-regexp-face ((,class (:foreground ,teal-30))))
   `(font-lock-warning-face ((,class (:foreground ,magenta-50))))
   `(font-lock-bracket-face ((,class (:foreground ,white))))
   `(font-lock-delimiter-face ((,class (:foreground ,white))))
   `(font-lock-doc-markup-face ((,class (:foreground ,gray-70))))
   `(font-lock-preprocessor-face ((,class (:foreground ,blue-40))))
   `(font-lock-punctuation-face ((,class (:foreground ,white))))
   `(font-lock-negation-char-face ((,class (:foreground ,blue-40))))
   `(font-lock-operator-face ((,class (:foreground ,blue-40))))

   ;; helpful
   `(helpful-heading ((,class (:foreground ,blue-40 :weight bold))))

   ;; corfu
   `(corfu-default ((,class (:background ,gray-90 :foreground ,white))))
   `(corfu-current ((,class (:background ,gray-80 :foreground ,magenta-50 :weight bold))))
   `(corfu-bar ((,class (:background ,gray-70))))
   `(corfu--bar ((,class (:background ,gray-70))))
   `(corfu-border ((,class (:background ,gray-70))))
   `(corfu-echo ((,class (:foreground ,magenta-40))))
   `(corfu-annotation ((,class (:foreground ,gray-40))))
   `(corfu-deprecated ((,class (:foreground ,gray-60 :strike-through t))))
   
   ;; vertico
   `(vertico-current ((,class (:background ,gray-90 :foreground ,magenta-50 :weight bold))))
   `(vertico-group-title ((,class (:foreground ,magenta-50 :weight bold))))
   `(vertico-group-separator ((,class (:foreground ,gray-50 :strike-through t))))
   `(vertico-multiline ((,class (:foreground ,gray-40))))
   `(marginalia-documentation ((,class (:foreground ,gray-30))))

   ;; embark
   `(embark-keymap ((,class (:foreground ,magenta-50))))
   `(embark-keybinding ((,class (:foreground ,blue-40))))

   ;; magit
   `(magit-section-heading ((,class (:foreground ,magenta-50 :weight bold))))
   `(magit-section-highlight ((,class (:background ,gray-90))))
   `(magit-branch-remote ((,class (:foreground ,green-40))))
   `(magit-branch-local ((,class (:foreground ,magenta-40))))
   `(magit-tag ((,class (:foreground ,blue-40))))
   `(magit-dimmed ((,class (:foreground ,gray-50))))
   `(magit-hash ((,class (:foreground ,gray-40))))
   `(magit-header-line ((,class (:background ,gray-90 :foreground ,white :weight bold))))
   `(magit-head ((,class (:foreground ,magenta-50 :weight bold))))
   `(magit-reflog-commit ((,class (:foreground ,green-40))))
   `(magit-reflog-merge ((,class (:foreground ,purple-50))))
   `(magit-reflog-checkout ((,class (:foreground ,magenta-40))))
   `(magit-reflog-reset ((,class (:foreground ,red-50))))
   `(magit-reflog-rebase ((,class (:foreground ,magenta-50))))
   `(magit-blame-heading ((,class (:background ,gray-90 :foreground ,white))))
   `(magit-blame-date ((,class (:foreground ,blue-40))))
   `(magit-blame-hash ((,class (:foreground ,magenta-50))))
   `(magit-diff-added ((,class (:foreground ,green-40))))
   `(magit-diff-added-highlight ((,class (:foreground ,green-40 :background ,gray-90))))
   `(magit-diff-removed ((,class (:foreground ,red-50))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red-50 :background ,gray-90))))
   `(magit-diff-context ((,class (:foreground ,gray-40))))
   `(magit-diff-context-highlight ((,class (:foreground ,gray-40 :background ,gray-90))))

   ;; forge
   `(forge-topic-label ((,class (:box (:line-width -1 :color ,gray-70)))))
   `(forge-post-author ((,class (:foreground ,blue-40 :weight bold))))
   `(forge-post-date ((,class (:foreground ,gray-40))))

   ;; code-review
   `(code-review-timestamp-face ((,class (:foreground ,gray-40))))
   `(code-review-author-face ((,class (:foreground ,blue-40))))
   `(code-review-comment-face ((,class (:foreground ,purple-50))))

   ;; Rainbow delimiters - Revised color sequence
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,magenta-50))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,blue-40))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyan-40))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,purple-50))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,teal-40))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,magenta-40))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue-60))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cyan-50))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,purple-50))))
   `(rainbow-delimiters-mismatched-face ((,class (:foreground ,red-50 :weight bold))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red-50 :weight bold)))))
  )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'carbon)
(provide 'carbon-theme)

(comment
 (let ((themes custom-enabled-themes))
   (mapc #'disable-theme custom-enabled-themes)
   (load-theme (car themes) t))
 )

;;; carbon-theme.el ends here
