;;; color-theme-sanityinc-solarized --- an alternate formulation of Ethan Schoonover's Solarized theme

;; Copyright (C) 2011 Steve Purcell

;; Author: Steve Purcell <steve [at] sanityinc.com>
;; Keywords: themes
;; X-URL: http://github.com/purcell/color-theme-sanityinc-solarized
;; URL: http://github.com/purcell/color-theme-sanityinc-solarized

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
;;
;; Here are two slightly subdued color themes that are easy on the eyes
;; and cover a reasonably complete set of faces.
;;
;;; Use:
;;
;; M-x color-theme-solarized-light
;; M-x color-theme-solarized-dark
;;
;;; Credit:
;;
;; Genius colour selection by Ethan Schoonover:
;; http://ethanschoonover.com/solarized
;; Some faces borrowed from Greg Pfeil's emacs theme:
;; https://github.com/sellout/solarized/blob/master/emacs-color-theme-solarized/color-theme-solarized.el
;;
;;; Code:

;; requires
(require 'color-theme)


;;;###autoload
(defun color-theme-sanityinc-solarized (mode)
  (interactive "Sdark or light")
  (let* ((base03  "#002b36")
         (base02  "#073642")
         (base01  "#586e75")
         (base00  "#657b83")
         (base0   "#839496")
         (base1   "#93a1a1")
         (base2   "#eee8d5")
         (base3   "#fdf6e3")
         (yellow  "#b58900")
         (orange  "#cb4b16")
         (red     "#d30102")
         (magenta "#d33682")
         (violet  "#6c71c4")
         (blue    "#268bd2")
         (cyan    "#2aa198")
         (green   "#859900"))
    (when (and (eq system-type 'darwin) (eq 'ns window-system))
      ;; Colors patched by using OS X ColorSync Utility to convert
      ;; from sRGB to colors in the Generic RGB which Emacs implicitly uses
      (setq base03  "#042028" ; (0.0159 0.1265 0.1597)
            base02  "#0a2832" ; (0.0394 0.1601 0.1983)
            base01  "#465a61" ; (0.2767 0.3567 0.3830)
            base00  "#52676f" ; (0.3244 0.4072 0.4385)
            base0   "#708183" ; (0.4406 0.5096 0.5169)
            base1   "#81908f" ; (0.5060 0.5649 0.5636)
            base2   "#e9e2cb" ; (0.9161 0.8900 0.7978)
            base3   "#fcf4dc" ; (0.9894 0.9579 0.8641)
            yellow  "#a57705" ; (0.6475 0.4675 0.0235)
            orange  "#bd3612" ; (0.7418 0.2133 0.0735)
            red     "#c60007" ; (0.7770 0.0000 0.0290)
            magenta "#c61b6e" ; (0.7774 0.1080 0.4352)
            violet  "#5859b7" ; (0.3479 0.3514 0.7179)
            blue    "#2075c7" ; (0.1275 0.4627 0.7823)
            cyan    "#259185" ; (0.1468 0.5708 0.5250)
            green   "#728a05" ; (0.4498 0.5412 0.0202)
            ))
    (let ((foregrounds (list base1 base0 base00 base01))
          (backgrounds (list base03 base02))
          (contrast-backgrounds (list base3 base2)))
      (when (eq 'light mode)
        (rotatef backgrounds contrast-backgrounds)
        (setq foregrounds (reverse foregrounds)))
      (let ((background (first backgrounds))
            (foreground (second foregrounds))
            (alt-background (second backgrounds))
            (contrast-background (first contrast-backgrounds)))
        (color-theme-install
         `(,(intern (concat "color-theme-sanityinc-solarized-" (symbol-name'light)))
           ((background-color . ,background)
            (background-mode . light)
            (border-color . ,foreground)
            (cursor-color . ,magenta)
            (foreground-color . ,foreground)
            (mouse-color . ,cyan))

           ;; Standard font lock faces
           (default ((t (nil))))
           (bold ((t (:bold t))))
           (bold-italic ((t (:italic t :bold t))))
           (underline ((t (:underline t))))
           (italic ((t (:italic t))))
           (font-lock-builtin-face ((t (:foreground ,green))))
           (font-lock-comment-delimiter-face ((t (:foreground ,(third foregrounds) :slant italic))))
           (font-lock-comment-face ((t (:foreground ,(fourth foregrounds) :slant italic))))
           (font-lock-constant-face ((t (:foreground ,yellow))))
           (font-lock-doc-face ((t (:foreground ,magenta))))
           (font-lock-doc-string-face ((t (:foreground ,yellow))))
           (font-lock-function-name-face ((t (:foreground ,blue))))
           (font-lock-keyword-face ((t (:foreground ,green :bold t))))
           (font-lock-preprocessor-face ((t (:foreground ,orange))))
           (font-lock-string-face ((t (:foreground ,cyan))))
           (font-lock-type-face ((t (:foreground ,yellow))))
           (font-lock-variable-name-face ((t (:foreground ,orange))))
           (font-lock-warning-face ((t (:bold t :foreground ,red))))

           ;; Flymake
           (flymake-warnline ((t (:underline ,orange :background ,background))))
           (flymake-errline ((t (:underline ,red :background ,background))))

           ;; MMM-mode
           (mmm-code-submode-face ((t (:background ,alt-background))))
           (mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
           (mmm-output-submode-face ((t (:background ,alt-background))))

           ;; Search
           (isearch ((t (:foreground ,background :background ,green))))
           (isearch-lazy-highlight-face ((t (:foreground ,foreground :background ,yellow))))

           ;; IDO
           (ido-subdir ((t (:foreground ,magenta))))
           (ido-first-match ((t (:foreground ,orange))))
           (ido-only-match ((t (:foreground ,green))))

           ;; Emacs interface
           (fringe ((t (:background ,alt-background))))
           (border ((t (:background ,alt-background))))
           (border-glyph ((t (nil))))
           (highlight ((t (:background ,(second contrast-backgrounds)))))
           (gui-element ((t (:background ,alt-background :foreground ,foreground))))
           (mode-line ((t (:foreground ,(fourth foregrounds) :background ,alt-background :bold t
                                       :box (:line-width 1)))))
           (mode-line-buffer-id ((t (:foreground nil :background nil :bold nil))))
           (mode-line-inactive ((t (:inherit mode-line
                                             :foreground ,(third foregrounds)
                                             :background ,background :bold nil
                                             :box (:line-width 1)))))
           (minibuffer-prompt ((t (:foreground ,blue))))
           (region ((t (:background ,(second contrast-backgrounds)))))
           (secondary-selection ((t (:foreground ,(second foregrounds)))))

           ;; Parenthesis matching
           (show-paren-match ((t (:background nil :foreground nil :inverse-video t))))
           (show-paren-mismatch ((t (:background ,magenta :foreground ,background))))

           (slime-highlight-edits-face ((t (:foreground ,(first foregrounds)))))
           (magit-item-highlight ((t (:inherit highlight :background nil))))

           (link ((t (:foreground nil :underline t))))
           (widget-button ((t (:underline t))))
           (org-link ((t (:foreground ,blue :underline t))))
           (org-date ((t (:foreground ,blue :underline t))))
           (org-done ((t (:foreground ,green))))
           (org-todo ((t (:foreground ,red))))
           (org-special-keyword ((t (:foreground ,orange))))
           (org-level-1 ((t (:foreground ,blue))))
           (org-level-2 ((t (:foreground ,foreground))))
           (org-level-3 ((t (:foreground ,violet))))
           (org-column ((t (:background ,alt-background))))
           (org-warning ((t (:bold t :foreground ,red))))
           (org-scheduled-previously ((t (:foreground ,orange))))

           (hl-sexp-face ((t (:background ,alt-background))))
           (highlight-80+ ((t (:background ,alt-background))))
           ))))))

;;;###autoload
(defun color-theme-sanityinc-solarized-dark ()
  (interactive)
  (color-theme-sanityinc-solarized 'dark))

;;;###autoload
(defun color-theme-sanityinc-solarized-light ()
  (interactive)
  (color-theme-sanityinc-solarized 'light))


(provide 'color-theme-sanityinc-solarized)
