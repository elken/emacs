;;; doom-carbon-theme.el --- Carbon theme for doom-themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'doom-themes)

;;
;;; Theme definition

(def-doom-theme doom-carbon
  "A dark theme inspired by IBM Carbon Design System with purple as primary."

  ;; name        default   256       16
  ((bg         '("#000000" nil       nil            ))
   (bg-alt     '("#161616" nil       nil            ))
   (base0      '("#262626" "#262626" "black"        ))
   (base1      '("#393939" "#393939" "brightblack"  ))
   (base2      '("#525252" "#525252" "brightblack"  ))
   (base3      '("#6f6f6f" "#6f6f6f" "brightblack"  ))
   (base4      '("#8d8d8d" "#8d8d8d" "brightblack"  ))
   (base5      '("#a8a8a8" "#a8a8a8" "brightblack"  ))
   (base6      '("#c6c6c6" "#c6c6c6" "brightblack"  ))
   (base7      '("#e0e0e0" "#e0e0e0" "brightblack"  ))
   (base8      '("#f4f4f4" "#f4f4f4" "white"        ))
   (fg         '("#f4f4f4" "#f4f4f4" "white"        ))
   (fg-alt     '("#c6c6c6" "#c6c6c6" "brightwhite"  ))

   (grey       base4)
   (red        '("#fa4d56" "#fa4d56" "red"          ))
   (orange     '("#ff832b" "#ff832b" "brightred"    ))
   (green      '("#42be65" "#42be65" "green"        ))
   (teal       '("#08bdba" "#08bdba" "brightgreen"  ))
   (yellow     '("#ffb000" "#ffb000" "yellow"       ))
   (blue       '("#78a9ff" "#78a9ff" "brightblue"   ))
   (dark-blue  '("#4589ff" "#4589ff" "blue"         ))
   (magenta    '("#ff7eb6" "#ff7eb6" "brightmagenta"))
   (violet     '("#ff7eb6" "#ff7eb6" "magenta"      ))
   (cyan       '("#82cfff" "#82cfff" "brightcyan"   ))
   (dark-cyan  '("#1192e8" "#1192e8" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      base1)
   (builtin        violet)
   (comments       base3)
   (doc-comments   base3)
   (constants      violet)
   (functions      violet)
   (keywords       blue)
   (methods        violet)
   (operators      magenta)
   (type           magenta)
   (strings        green)
   (variables      (doom-lighten fg 0.1))
   (numbers        violet)
   (region         base1)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))

   (modeline-fg     fg)
   (modeline-fg-alt base5)
   (modeline-bg     base0)
   (modeline-bg-l   base1)
   (modeline-bg-inactive   base0)
   (modeline-bg-inactive-l base1))

  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground violet)

   (font-lock-comment-face
    :foreground comments)
   (font-lock-doc-face
    :foreground doc-comments)
   (font-lock-keyword-face
    :foreground keywords
    :weight 'bold)
   (font-lock-builtin-face
    :foreground builtin
    :weight 'bold)
   (font-lock-function-name-face
    :foreground functions
    :weight 'bold)
   (font-lock-variable-name-face
    :foreground variables)
   (font-lock-string-face
    :foreground strings)
   (font-lock-type-face
    :foreground type
    :weight 'bold)
   (font-lock-constant-face
    :foreground constants
    :weight 'bold)
   (font-lock-number-face
    :foreground numbers)
   (font-lock-operator-face
    :foreground operators)
   (font-lock-property-face
    :foreground violet)
   (font-lock-bracket-face
    :foreground base6)
   (font-lock-delimiter-face
    :foreground base6)
   (font-lock-punctuation-face
    :foreground base6)
   (font-lock-preprocessor-face
    :foreground magenta)
   (font-lock-regexp-grouping-backslash
    :foreground orange)
   (font-lock-regexp-grouping-construct
    :foreground orange)
   (font-lock-escape-face
    :foreground orange)

   (mode-line
    :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt)
   (mode-line-emphasis
    :foreground highlight)

   (solaire-default-face :inherit 'default :background bg-alt)
   (solaire-hl-line-face :inherit 'hl-line :background bg)

   ;;;; CSS mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;;;; doom-modeline
   (doom-modeline-bar :background highlight)

   ;;;; ediff <built-in>
   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.1))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.1))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.1))
   (ediff-current-diff-Ancestor :foreground violet :background (doom-lighten violet 0.1))

   ;;;; helm
   (helm-candidate-number :background blue :foreground bg)

   ;;;; lsp-mode
   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend violet bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)

   ;;;; magit
   (magit-blame-heading     :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face   :background (doom-lighten base3 0.05))

   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)

   ;;;; org <built-in>
   (org-hide :foreground hidden)
   (org-block :background bg-alt :foreground fg)
   (org-block-begin-line :foreground base4 :background bg-alt)
   (org-block-end-line :foreground base4 :background bg-alt)
   (org-document-title :foreground violet :weight 'bold :height 1.4)
   (org-document-info :foreground cyan)
   (org-document-info-keyword :foreground base4)
   (org-level-1 :foreground violet :weight 'bold :height 1.3)
   (org-level-2 :foreground cyan :weight 'bold :height 1.2)
   (org-level-3 :foreground blue :weight 'bold :height 1.1)
   (org-level-4 :foreground magenta :weight 'bold)
   (org-level-5 :foreground green :weight 'bold)
   (org-level-6 :foreground yellow :weight 'bold)
   (org-level-7 :foreground orange :weight 'bold)
   (org-level-8 :foreground red :weight 'bold)
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground green :weight 'bold)
   (org-headline-done :foreground base4)
   (org-priority :foreground orange :weight 'bold)
   (org-tag :foreground base5)
   (org-date :foreground cyan :weight 'bold)
   (org-scheduled :foreground green :weight 'bold)
   (org-scheduled-today :foreground violet :weight 'bold)
   (org-scheduled-previously :foreground orange :weight 'bold)
   (org-deadline :foreground red :weight 'bold)
   (org-upcoming-deadline :foreground orange :weight 'bold)
   (org-time-grid :foreground base4)
   (org-special-keyword :foreground base4)
   (org-drawer :foreground base4)
   (org-property-value :foreground base5)
   (org-table :foreground base6)
   (org-link :foreground violet :underline t)
   (org-footnote :foreground cyan)
   (org-code :foreground cyan)
   (org-verbatim :foreground green)
   (org-quote :foreground base5 :slant 'italic)
   (org-meta-line :foreground base4)
   (org-checkbox :foreground violet)
   (org-checkbox-statistics-todo :foreground red)
   (org-checkbox-statistics-done :foreground green)

   ;; org-modern specific faces
   (org-modern-label :height 0.9 :width 'condensed :weight 'regular :underline nil
                     :foreground fg :distant-foreground bg)
   (org-modern-tag :inherit '(org-modern-label)
                   :foreground bg :background base5 :weight 'bold)
   (org-modern-todo :inherit '(org-modern-label)
                    :foreground bg :background red :weight 'bold)
   (org-modern-done :inherit '(org-modern-label)
                    :foreground bg :background green :weight 'bold)
   (org-modern-priority :inherit '(org-modern-label)
                        :foreground bg :background orange :weight 'bold)
   (org-modern-date-active :inherit '(org-modern-label)
                           :foreground bg :background cyan :weight 'bold)
   (org-modern-date-inactive :inherit '(org-modern-label)
                             :foreground base4 :background base1)
   (org-modern-time-active :inherit '(org-modern-label)
                           :foreground bg :background violet :weight 'bold)
   (org-modern-time-inactive :inherit '(org-modern-label)
                             :foreground base4 :background base1)
   (org-modern-block-name :foreground base4 :height 0.8 :weight 'light)
   (org-modern-symbol :foreground base5)
   (org-modern-horizontal-rule :inherit 'org-hide :strike-through base3)

   (solaire-org-hide-face :foreground hidden))

  ;; --- extra variables ---------------------
  ()
  )

;;; doom-carbon-theme.el ends here
