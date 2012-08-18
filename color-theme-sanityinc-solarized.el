;;; color-theme-sanityinc-solarized.el --- A version of Ethan Schoonover's Solarized themes

;; Copyright (C) 2011 Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; Version: {{VERSION}}

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Here are two slightly subdued color themes that are easy on the eyes
;; and cover a reasonably complete set of faces.

;; In Emacs versions without built-in theme support, ie. < 24,
;; color-theme.el is required.

;; Use:

;;   M-x color-theme-solarized-light

;;   M-x color-theme-solarized-dark
;;
;;; Credit:

;; Genius colour selection by Ethan Schoonover:
;; http://ethanschoonover.com/solarized

;; Some faces borrowed from Greg Pfeil's emacs theme:
;; https://github.com/sellout/solarized/blob/master/emacs-color-theme-solarized/color-theme-solarized.el

;;; Code:

(require 'cl)

(defmacro color-theme-sanityinc-solarized--with-colors (mode &rest body)
  "Execute `BODY' in a scope with variables bound to the various solarized colors.

`MODE' should be set to either 'light or 'dark."
  ;; These are the Generic RGB equivalents of the "official" sRGB hex values
  `(let* ((base03  "#042028")            ; (0.0159 0.1265 0.1597)
          (base02  "#0a2832")            ; (0.0394 0.1601 0.1983)
          (base01  "#465a61")            ; (0.2767 0.3567 0.3830)
          (base00  "#52676f")            ; (0.3244 0.4072 0.4385)
          (base0   "#708183")            ; (0.4406 0.5096 0.5169)
          (base1   "#81908f")            ; (0.5060 0.5649 0.5636)
          (base2   "#e9e2cb")            ; (0.9161 0.8900 0.7978)
          (base3   "#fcf4dc")            ; (0.9894 0.9579 0.8641)
          (yellow  "#a57705")            ; (0.6475 0.4675 0.0235)
          (orange  "#bd3612")            ; (0.7418 0.2133 0.0735)
          (red     "#c60007")            ; (0.7770 0.0000 0.0290)
          (magenta "#c61b6e")            ; (0.7774 0.1080 0.4352)
          (violet  "#5859b7")            ; (0.3479 0.3514 0.7179)
          (blue    "#2075c7")            ; (0.1275 0.4627 0.7823)
          (cyan    "#259185")            ; (0.1468 0.5708 0.5250)
          (green   "#728a05")            ; (0.4498 0.5412 0.0202)
          (foregrounds (list base1 base0 base00 base01))
          (backgrounds (list base03 base02))
          (contrast-backgrounds (list base3 base2)))
     (when (eq 'light ,mode)
       (rotatef backgrounds contrast-backgrounds)
       (setq foregrounds (reverse foregrounds)))
     (let ((background (nth 0 backgrounds))
           (alt-background (nth 1 backgrounds))
           (strong (nth 0 foregrounds))
           (normal (nth 1 foregrounds))
           (faint (nth 2 foregrounds))
           (faintest (nth 3 foregrounds))
           (contrast-background (nth 1 contrast-backgrounds))
           (class '((class color) (min-colors 89))))
       ,@body)))

(defmacro color-theme-sanityinc-solarized--face-specs ()
  "Return a backquote which defines a list of face specs.

It expects to be evaluated in a scope in which the various color
names to which it refers are bound."
  (quote
   `(;; Standard font lock faces
     (default ((,class (:foreground ,normal :background ,background))))
     (bold ((,class (:weight bold))))
     (bold-italic ((,class (:slant italic :weight bold))))
     (underline ((,class (:underline t))))
     (italic ((,class (:slant italic))))
     (font-lock-builtin-face ((,class (:foreground ,violet))))
     (font-lock-comment-delimiter-face ((,class (:foreground ,faintest :slant italic))))
     (font-lock-comment-face ((,class (:foreground ,faint :slant italic))))
     (font-lock-constant-face ((,class (:foreground ,violet))))
     (font-lock-doc-face ((,class (:foreground ,magenta))))
     (font-lock-doc-string-face ((,class (:foreground ,yellow))))
     (font-lock-function-name-face ((,class (:foreground ,blue))))
     (font-lock-keyword-face ((,class (:foreground ,green))))
     (font-lock-negation-char-face ((,class (:foreground ,green))))
     (font-lock-preprocessor-face ((,class (:foreground ,magenta))))
     (font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))
     (font-lock-regexp-grouping-construct ((,class (:foreground ,violet))))
     (font-lock-string-face ((,class (:foreground ,cyan))))
     (font-lock-type-face ((,class (:foreground ,yellow))))
     (font-lock-variable-name-face ((,class (:foreground ,yellow))))
     (font-lock-warning-face ((,class (:weight bold :foreground ,red))))
     (shadow ((,class (:foreground ,(fourth foregrounds)))))
     (success ((,class (:foreground ,green))))
     (error ((,class (:foreground ,red))))
     (warning ((,class (:foreground ,orange))))

     ;; Flymake
     (flymake-warnline ((,class (:underline ,orange :background ,background))))
     (flymake-errline ((,class (:underline ,red :background ,background))))

     ;; Clojure errors
     (clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))))
     (clojure-test-error-face ((,class (:background nil :inherit flymake-errline))))
     (clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))))

     ;; For Brian Carper's extended clojure syntax table
     (clojure-keyword ((,class (:foreground ,yellow))))
     (clojure-parens ((,class (:foreground ,strong))))
     (clojure-braces ((,class (:foreground ,green))))
     (clojure-brackets ((,class (:foreground ,yellow))))
     (clojure-double-quote ((,class (:foreground ,cyan :background nil))))
     (clojure-special ((,class (:foreground ,blue))))
     (clojure-java-call ((,class (:foreground ,magenta))))

     ;; Rainbow-delimiters
     (rainbow-delimiters-depth-1-face ((,class (:foreground ,normal))))
     (rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-4-face ((,class (:foreground ,green))))
     (rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
     (rainbow-delimiters-depth-6-face ((,class (:foreground ,normal))))
     (rainbow-delimiters-depth-7-face ((,class (:foreground ,cyan))))
     (rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
     (rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
     (rainbow-delimiters-unmatched-face ((,class (:foreground ,red))))

     ;; MMM-mode
     (mmm-code-submode-face ((,class (:background ,alt-background))))
     (mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))))
     (mmm-output-submode-face ((,class (:background ,alt-background))))

     ;; Search
     (match ((,class (:foreground ,blue :background ,background :inverse-video t))))
     (isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))))
     (isearch-lazy-highlight-face ((,class (:foreground ,cyan :background ,background :inverse-video t))))
     (isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))))

     ;; IDO
     (ido-subdir ((,class (:foreground ,magenta))))
     (ido-first-match ((,class (:foreground ,yellow))))
     (ido-only-match ((,class (:foreground ,green))))
     (ido-indicator ((,class (:foreground ,red :background ,background))))
     (ido-virtual ((,class (:foreground ,faintest))))

     ;; Emacs interface
     (cursor ((,class (:background ,magenta))))
     (fringe ((,class (:background ,alt-background))))
     (linum ((,class (:background ,alt-background))))
     (border ((,class (:background ,alt-background))))
     (border-glyph ((,class (nil))))
     (highlight ((,class (:inverse-video nil :background ,alt-background))))
     (gui-element ((,class (:background ,alt-background :foreground ,normal))))
     (mode-line ((,class (:foreground nil :background ,alt-background :weight bold
                                 :box (:line-width 1 :color ,normal)))))
     (mode-line-buffer-id ((,class (:foreground ,magenta :background nil))))
     (mode-line-inactive ((,class (:inherit mode-line
                                       :foreground ,faintest
                                       :background ,alt-background :weight normal
                                       :box (:line-width 1 :color ,normal)))))
     (mode-line-emphasis ((,class (:foreground ,strong))))
     (mode-line-highlight ((,class (:foreground ,magenta :box nil :weight bold))))
     (minibuffer-prompt ((,class (:foreground ,blue))))
     (region ((,class (:background ,contrast-background))))
     (secondary-selection ((,class (:background ,alt-background))))

     (header-line ((,class (:background nil :foreground ,strong :inherit nil))))
     (trailing-whitespace ((,class (:background ,red :underline nil))))

     ;; Parenthesis matching (built-in)
     (show-paren-match ((,class (:background nil :foreground nil :inverse-video t))))
     (show-paren-mismatch ((,class (:background ,magenta :foreground ,background))))

     ;; Parenthesis matching (mic-paren)
     (paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))))
     (paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))
     (paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))))

     ;; Parenthesis dimming (parenface)
     (paren-face ((,class (:foreground ,faintest :background nil))))

     (sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))))
     (sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))))
     (slime-highlight-edits-face ((,class (:foreground ,strong))))
     (slime-repl-input-face ((,class (:weight normal :underline nil))))
     (slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,magenta))))
     (slime-repl-result-face ((,class (:foreground ,green))))
     (slime-repl-output-face ((,class (:foreground ,blue :background ,background))))

     (csv-separator-face ((,class (:foreground ,yellow))))

     (diff-added ((,class (:foreground ,green))))
     (diff-changed ((,class (:foreground ,violet))))
     (diff-removed ((,class (:foreground ,orange))))
     (diff-header ((,class (:foreground ,cyan :background nil))))
     (diff-file-header ((,class (:foreground ,blue :background nil))))
     (diff-hunk-header ((,class (:foreground ,magenta))))

     ;; undo-tree
     (undo-tree-visualizer-default-face ((,class (:foreground ,normal))))
     (undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))))
     (undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
     (undo-tree-visualizer-register-face ((,class (:foreground ,yellow))))

     ;; dired+
     (diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))))
     (diredp-dir-priv ((,class (:foreground ,cyan :background nil))))
     (diredp-exec-priv ((,class (:foreground ,blue :background nil))))
     (diredp-file-name ((,class (:foreground ,yellow))))
     (diredp-file-suffix ((,class (:foreground ,green))))
     (diredp-flag-mark-line ((,class (:background nil :inherit highlight))))
     (diredp-ignored-file-name ((,class (:foreground ,faintest))))
     (diredp-link-priv ((,class (:background nil :foreground ,violet))))
     (diredp-no-priv ((,class (:background nil))))
     (diredp-number ((,class (:foreground ,yellow))))
     (diredp-other-priv ((,class (:background nil :foreground ,magenta))))
     (diredp-rare-priv ((,class (:foreground ,red :background nil))))
     (diredp-read-priv ((,class (:foreground ,green :background nil))))
     (diredp-symlink ((,class (:foreground ,violet))))
     (diredp-write-priv ((,class (:foreground ,yellow :background nil))))

     ;; Magit (a patch is pending in magit to make these standard upstream)
     (magit-branch ((,class (:foreground ,green))))
     (magit-item-highlight ((,class (:inherit highlight :background nil))))
     (magit-log-graph ((,class (:foreground ,faintest))))
     (magit-log-sha1 ((,class (:foreground ,yellow))))
     (magit-log-head-label-bisect-bad ((,class (:foreground ,red))))
     (magit-log-head-label-bisect-good ((,class (:foreground ,green))))
     (magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))))
     (magit-log-head-label-local ((,class (:foreground ,magenta :box nil :weight bold))))
     (magit-log-head-label-remote ((,class (:foreground ,violet :box nil :weight bold))))
     (magit-log-head-label-tags ((,class (:foreground ,cyan :box nil :weight bold))))
     (magit-section-title ((,class (:foreground ,blue :weight bold))))

     (link ((,class (:foreground nil :underline t))))
     (widget-button ((,class (:underline t))))
     (widget-field ((,class (:background ,alt-background :box (:line-width 1 :color ,normal)))))

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
     (compilation-column-number ((,class (:foreground ,yellow))))
     (compilation-line-number ((,class (:foreground ,yellow))))
     (compilation-message-face ((,class (:foreground ,blue))))

     ;; Grep
     (grep-context-face ((,class (:foreground ,faint))))
     (grep-error-face ((,class (:foreground ,red :weight bold :underline t))))
     (grep-hit-face ((,class (:foreground ,blue))))
     (grep-match-face ((,class (:foreground nil :background nil :inherit match))))

     ;; Term
     (term-color-black ((,class (:background ,base02 :foreground ,base02))))
     (term-color-blue ((,class (:background ,blue :foreground ,blue))))
     (term-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
     (term-color-green ((,class (:background ,green :foreground ,green))))
     (term-color-magenta ((,class (:background ,magenta :foreground ,magenta))))
     (term-color-red ((,class (:background ,red :foreground ,red))))
     (term-color-white ((,class (:background ,base2 :foreground ,base2))))
     (term-color-yellow ((,class (:background ,yellow :foreground ,yellow))))

     ;; Eshell
     (eshell-ls-archive ((,class (:foreground ,cyan :weight normal))))
     (eshell-ls-backup ((,class (:foreground ,yellow))))
     (eshell-ls-clutter ((,class (:foreground ,orange :weight normal))))
     (eshell-ls-directory ((,class (:foreground ,blue :weight normal))))
     (eshell-ls-executable ((,class (:foreground ,red :weight normal))))
     (eshell-ls-missing ((,class (:foreground ,violet :weight normal))))
     (eshell-ls-product ((,class (:foreground ,yellow))))
     (eshell-ls-readonly ((,class (:foreground ,base1))))
     (eshell-ls-special ((,class (:foreground ,green :weight normal))))
     (eshell-ls-symlink ((,class (:foreground ,magenta :weight normal))))
     (eshell-ls-unreadable ((,class (:foreground ,base00))))
     (eshell-prompt ((,class (:foreground ,green :weight normal))))

     (regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))))

     ;; Stop outline-3 from inheriting font-lock-keyword-face, which we've made bold
     (outline-3 ((,class (:inherit nil :foreground ,green))))

     (org-date ((,class (:foreground ,blue :underline t))))
     (org-agenda-structure ((,class (:foreground ,violet))))
     (org-agenda-date ((,class (:foreground ,blue :underline nil))))
     (org-agenda-dimmed-todo-face ((,class (:foreground ,faint))))
     (org-block ((,class (:foreground ,orange))))
     (org-code ((,class (:foreground ,yellow))))
     (org-column ((,class (:background ,alt-background))))
     (org-document-info ((,class (:foreground ,cyan))))
     (org-document-info-keyword ((,class (:foreground ,green))))
     (org-document-title ((,class (:weight bold :foreground ,yellow :height 1.44))))
     (org-done ((,class (:foreground ,green))))
     (org-formula ((,class (:foreground ,orange))))
     (org-link ((,class (:foreground ,blue :underline t))))
     (org-scheduled ((,class (:foreground ,green))))
     (org-scheduled-previously ((,class (:foreground ,yellow))))
     (org-scheduled-today ((,class (:foreground ,green))))
     (org-special-keyword ((,class (:foreground ,yellow))))
     (org-table ((,class (:foreground ,violet))))
     (org-todo ((,class (:foreground ,red))))
     (org-upcoming-deadline ((,class (:foreground ,yellow))))
     (org-warning ((,class (:weight bold :foreground ,red))))

     (markdown-header-face ((,class (:inherit header-line))))
     (markdown-url-face ((,class (:inherit link))))
     (markdown-link-face ((,class (:foreground ,blue :underline t))))

     (hl-sexp-face ((,class (:background ,alt-background))))
     (highlight-80+ ((,class (:background ,alt-background))))

     ;; Python-specific overrides
     (py-builtins-face ((,class (:foreground ,orange :weight normal))))

     ;; js2-mode
     (js2-warning-face ((,class (:underline ,yellow))))
     (js2-error-face ((,class (:underline ,red))))
     (js2-external-variable-face ((,class (:foreground ,magenta))))
     (js2-function-param-face ((,class (:foreground ,blue))))
     (js2-instance-member-face ((,class (:foreground ,blue))))
     (js2-private-function-call-face ((,class (:foreground ,red))))

     ;; js3-mode
     (js3-warning-face ((,class (:underline ,yellow))))
     (js3-error-face ((,class (:underline ,red))))
     (js3-external-variable-face ((,class (:foreground ,magenta))))
     (js3-function-param-face ((,class (:foreground ,blue))))
     (js3-jsdoc-tag-face ((,class (:foreground ,magenta))))
     (js3-jsdoc-type-face ((,class (:foreground ,cyan))))
     (js3-jsdoc-value-face ((,class (:foreground ,violet))))
     (js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))))
     (js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))))
     (js3-instance-member-face ((,class (:foreground ,blue))))
     (js3-private-function-call-face ((,class (:foreground ,red))))

     ;; nxml
     (nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))))
     (nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))))
     (nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))))
     (nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))))
     (nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))))
     (rng-error-face ((,class (:underline ,red))))

     ;; RHTML
     (erb-delim-face ((,class (:background ,alt-background))))
     (erb-exec-face ((,class (:background ,alt-background :weight bold))))
     (erb-exec-delim-face ((,class (:background ,alt-background))))
     (erb-out-face ((,class (:background ,alt-background :weight bold))))
     (erb-out-delim-face ((,class (:background ,alt-background))))
     (erb-comment-face ((,class (:background ,alt-background :weight bold :slant italic))))
     (erb-comment-delim-face ((,class (:background ,alt-background))))

     ;; Message-mode
     (message-header-other ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))))
     (message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))))
     (message-header-cc ((,class (:inherit message-header-to :foreground nil))))
     (message-header-name ((,class (:inherit header-line :foreground ,green :background nil))))
     (message-header-newsgroups ((,class (:foreground ,cyan :background nil :slant normal))))
     (message-separator ((,class (:foreground ,magenta))))

     ;; Gnus
     (gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))))
     (gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))))
     (gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))))
     (gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))))
     (gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))))
     (gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))))
     (gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))))
     (gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))))
     ;; there are several more -cite- faces...
     (gnus-header-content ((,class (:inherit header-line :foreground nil :background nil :weight normal))))
     (gnus-header-subject ((,class (:inherit gnus-header-content :weight bold :foreground ,yellow))))
     (gnus-header-from ((,class (:inherit gnus-header-content :weight bold :foreground ,orange))))
     (gnus-header-name ((,class (:inherit header-line :foreground ,green :background nil))))
     (gnus-button ((,class (:inherit link :foreground nil))))
     (gnus-signature ((,class (:inherit font-lock-comment-face))))

     (gnus-summary-normal-unread ((,class (:foreground ,strong :weight normal))))
     (gnus-summary-normal-read ((,class (:foreground ,normal :weight normal))))
     (gnus-summary-normal-ancient ((,class (:foreground ,cyan :weight normal))))
     (gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-low-unread ((,class (:foreground ,faint :weight normal))))
     (gnus-summary-low-read ((,class (:foreground ,faintest :weight normal))))
     (gnus-summary-low-ancient ((,class (:foreground ,faintest :weight normal))))
     (gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))))
     (gnus-summary-high-read ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))))
     (gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))))
     (gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))))

     (gnus-group-mail-low ((,class (:foreground ,faintest))))
     (gnus-group-mail-low-empty ((,class (:foreground ,faintest))))
     (gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))))
     (gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))))
     (gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))))
     (gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))))
     (gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))))
     (gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))))
     (gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,faint))))
     (gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,faint))))
     (gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,faint))))
     (gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,faint))))
     (gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,faint))))
     (gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,faint))))

     (erc-direct-msg-face ((,class (:foreground ,yellow))))
     (erc-error-face ((,class (:foreground ,red))))
     (erc-header-face ((,class (:foreground ,strong :background ,alt-background))))
     (erc-input-face ((,class (:foreground ,green))))
     (erc-current-nick-face ((,class (:foreground ,green))))
     (erc-my-nick-face ((,class (:foreground ,green))))
     (erc-nick-default-face ((,class (:weight normal :foreground ,violet))))
     (erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))))
     (erc-notice-face ((,class (:foreground ,faintest))))
     (erc-pal-face ((,class (:foreground ,orange))))
     (erc-prompt-face ((,class (:foreground ,blue))))
     (erc-timestamp-face ((,class (:foreground ,cyan))))

     (custom-variable-tag ((,class (:foreground ,blue))))
     (custom-group-tag ((,class (:foreground ,blue))))
     (custom-state-tag ((,class (:foreground ,green))))
     )))

(defmacro color-theme-sanityinc-solarized--frame-parameter-specs ()
  "Return a backquote which defines a list of frame parameter specs.

These are required by color-theme's `color-theme-install', but
not by the new `deftheme' mechanism. It expects to be evaluated
in a scope in which the various color names to which it refers
are bound."
  (quote
   `(((background-color . ,background)
      (background-mode . light)
      (border-color . ,normal)
      (cursor-color . ,magenta)
      (foreground-color . ,normal)
      (mouse-color . ,cyan)))))


(defmacro color-theme-sanityinc-solarized--define-theme (mode)
  "Define either the dark or the light theme."
  (let ((name (intern (format "sanityinc-solarized-%s" (symbol-name mode))))
        (doc (format "A version of Ethan Schoonover's 'Solarized' theme (%s version)" mode)))
    `(progn
       (deftheme ,name ,doc)
       (color-theme-sanityinc-solarized--with-colors
        ',mode
        (apply 'custom-theme-set-faces ',name
               (color-theme-sanityinc-solarized--face-specs))
        (custom-theme-set-variables
         ',name
         `(fci-rule-color ,alt-background)
         `(ansi-color-names-vector (vector ,normal ,red ,green ,yellow ,blue ,magenta ,cyan ,background))
         '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))
       (provide-theme ',name))))


(defun color-theme-sanityinc-solarized (mode)
  "Apply either the dark or the light theme."
  (if (fboundp 'load-theme)
      (let ((name (cond
                    ((eq 'light mode) 'sanityinc-solarized-light)
                    ((eq 'dark mode) 'sanityinc-solarized-dark)
                    (t (error "invalid mode: %s" mode)))))
        (if (> emacs-major-version 23)
            (load-theme name t)
          (load-theme name)))
    (progn
      (require 'color-theme)
      (color-theme-sanityinc-solarized--with-colors
       mode
       (color-theme-install
        `(,(intern (concat "color-theme-sanityinc-solarized-" (symbol-name mode)))
          ,@(color-theme-sanityinc-solarized--frame-parameter-specs)
          ,@(color-theme-sanityinc-solarized--face-specs)))
       ;; ansi-color - comint and other modes that handle terminal color escape sequences
       (setq ansi-color-names-vector (vector normal red green yellow blue magenta cyan background))
       (setq ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))))

;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun color-theme-sanityinc-solarized-dark ()
  (interactive)
  (color-theme-sanityinc-solarized 'dark))

;;;###autoload
(defun color-theme-sanityinc-solarized-light ()
  (interactive)
  (color-theme-sanityinc-solarized 'light))


(provide 'color-theme-sanityinc-solarized)
;;; color-theme-sanityinc-solarized.el ends here
