;;; oxocarbon-theme.el ---  -*- lexical-binding: t -*-
;; Copyright © 2023

;; URL: https://github.com/
;; Package-Requires: ((emacs "25") (autothemer "0.2"))
;; Created: 2023-06-24
;; Version: 2021-06-28
;; Keywords: faces

;; This file is NOT part of GNU Emacs.

;;; License:

;; MIT License

;; Copyright © 2022 Riccardo Mazzarini

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; A dark Emacs theme, inspired by IBM Carbon design system. 

;; Screenshots are available at: https://github.com/

;;; Code:

(require 'autothemer)

(autothemer-deftheme
 oxocarbon
 "A dark Emacs theme, inspired by IBM Carbon design systema with minor tweaks."
 ;; Specify the color classes used by the theme
  ((((class color) (min-colors #xFFFFFF))
    ((class color) (min-colors #xFF)))
     (base00  "#161616")
     (base01  "#262626")
     (base02  "#393939")
     (base03  "#525252")
     (base04  "#dde1e6")
     (base05  "#f2f4f8")
     (base06  "#ffffff")
     (base07  "#08bdba")
     (base07S "#023938")
     (base08  "#3ddbd9")
     (base09  "#78a9ff")
     (base0A  "#ee5396")
     (base0AS "#47192D")
     (base0B  "#33b1ff")
     (base0BS "#0F354D")
     (base0C  "#ff7eb6")
     (base0D  "#42be65")
     (base0E  "#be95ff")
     (base0ES "#392D4D")
     (base0F  "#82cfff")
     (baseEX  "#ffe97b")
     (baseEXS "#4D4625"))
  ((default             (:foreground base06 :background base00))
   (cursor              (:foreground base04 :background base04))
   (hl-line             (:background base01 :extend t))
   (region              (:foreground base06 :background base01))
   (lazy-highlight      (:foreground base06 :background base03))
   (secondary-selection (:foreground base06 :background base03))
   (highlight           (:foreground base06 :background base02))
   (fringe              (:foreground base06 :background base00))
   (match               (:foreground base0D))
   (scroll-bar          (:foreground base06 :background base00))
   (link                (:underline t))
   (link-visited        (:underline t :slant 'italic))
   (button              (:underline t))
   (tooltip             (:foreground base06 :background base01))
   (vertical-border     (:foreground base01 :background base01))
   (info-string         (:background base0F))
   (default-italic      (:slant 'italic))
   (error               (:inherit font-lock-warning-face))
   (warning             (:foreground base0A))
   (success             (:foreground base0D))
   (help-key-binding    (:foreground base06 :background base01))
   (cancel              (:foreground base0A :strike-through t))
   (font-lock-warning-face      (:foreground base0A :weight 'bold))
   (minibuffer-noticable-prompt (:foreground base06 :weight 'bold))
   (minibuffer-prompt           (:foreground base06 :weight 'bold))
   (isearch                     (:foreground base06 :background base0ES))
   (isearch-highlight           (:foreground base06 :background base01))
   (isearch-fail                (:foreground base0A :background base0AS))
   (show-paren-match            (:foreground base06 :background base02 :weight 'bold))
   (show-paren-match-expression (:foreground base06 :background base0ES))
   (show-paren-mismatch         (:foreground base0A :background base0AS))
   (paren-matched               (:foreground base06 :background base02))
   (paren-unmatched             (:foreground base0A :background base0AS))
   (escape-glyph                (:foreground base0A :weight 'bold))
   (homoglyph                   (:foreground base0A))
   ;; Syntax.
   (font-lock-comment-delimiter-face    (:foreground base03))
   (font-lock-comment-face              (:foreground base03))
   (font-lock-doc-face                  (:foreground base0E))
   (font-lock-preprocessor-face         (:foreground base06))
   (font-lock-preprocessor-char-face    (:foreground base06))
   (font-lock-regexp-grouping-backslash (:foreground base06 :background base02))
   (font-lock-regexp-grouping-construct (:foreground base06 :background base02))
   (font-lock-builtin-face              (:foreground base0E))
   (font-lock-constant-face             (:foreground base0E))
   (font-lock-function-name-face        (:foreground base0A :weight 'bold))
   (font-lock-keyword-face              (:foreground base07))
   (font-lock-type-face                 (:foreground base09))
   (font-lock-variable-name-face        (:foreground base06))
   (font-lock-string-face               (:foreground base0E))
   (font-lock-doc-markup-face           (:foreground base06 :background base01 :slant 'italic))
   ;; Delimiters.
   (rainbow-delimiters-depth-1-face     (:foreground base06))
   (rainbow-delimiters-depth-2-face     (:foreground base03))
   (rainbow-delimiters-depth-3-face     (:foreground base03))
   (rainbow-delimiters-depth-4-face     (:foreground base03))
   (rainbow-delimiters-depth-5-face     (:foreground base03))
   (rainbow-delimiters-depth-6-face     (:foreground base03))
   (rainbow-delimiters-depth-7-face     (:foreground base03))
   (rainbow-delimiters-depth-8-face     (:foreground base03))
   (rainbow-delimiters-depth-9-face     (:foreground base03))
   (rainbow-delimiters-depth-10-face    (:foreground base03))
   (rainbow-delimiters-depth-11-face    (:foreground base03))
   (rainbow-delimiters-depth-12-face    (:foreground base03))
   (rainbow-delimiters-unmatched-face   (:foreground base0A :background base0AS))
   ;; Elpaca
   (elpaca-blocked  (:foreground baseEX))
   (elpaca-busy     (:foreground base0C))
   (elpaca-failed   (:foreground base0A))
   (elpaca-finished (:foreground base0D))
   ;; Custom.
   (custom-invalid           (:foreground base06 :background base00  :underline (:style 'wave :color base0A)))
   (custom-rogue             (:foreground base06 :background base00 :underline nil))
   (custom-modified          (:foreground base06 :background base0F))
   (custom-set               (:foreground base06 :background base00 :weight 'bold))
   (custom-changed           (:foreground base06 :background base00 :weight 'bold))
   (custom-themed            (:foreground base06 :background base00))
   (custom-saved	         (:foreground base06 :background base00 :weight 'bold))
   (custom-state             (:foreground base06 :background base00 :slant 'italic))
   (custom-link              (:foreground base06 :background base00 :underline nil))
   (custom-visibility        (:foreground base06 :background base00 :height 0.8))
   (custom-comment           (:foreground base06 :background base00 :slant 'italic))
   (custom-comment-tag       (:foreground base06 :background base00 :slant 'italic))
   (custom-group-tag-1       (:foreground base06 :background base00 :weight 'bold :height 1.1))
   (custom-group-tag         (:foreground base06 :background base00 :weight 'bold :height 1.1))
   (custom-group-subtitle    (:foreground base06 :background base00 :weight 'bold))
   (custom-button            (:foreground base06 :background base01 :box (:line-width 1 :color base06)))
   (custom-button-mouse      (:foreground base06 :background base01 :box (:line-width 1 :color base01)))
   (custom-button-unraised   (:foreground base06 :background base01 :box (:line-width 1 :color base06)))
   (custom-variable-obsolete (:foreground base01 :background base00))
   (custom-variable-tag      (:foreground base06 :background base00))
   (custom-variable-button   (:foreground base06 :background base01 :box (:line-width 1 :color base06)))
   ;; Mode-line.
   (header-line         (:inherit 'mode-line :distant-foreground base01))
   (mode-line           (:foreground base06 :background base01 :box (:line-width 1 :color base01 :style nil)))
   (mode-line-inactive  (:foreground base03 :background base00 :box (:line-width 1 :color base00 :style nil)))
   (mode-line-buffer-id (:foreground base05 :bold t :distant-foreground base01))
   (mode-line-emphasis  (:foreground base05 :bold t))
   (mode-line-highlight (:foreground base05))
   ;; Doom-modeline.
   (doom-modeline-buffer-path        (:foreground base06))
   (doom-modeline-buffer-file        (:foreground base06 :weight 'bold))
   (doom-modeline-buffer-modified    (:foreground base0A :weight 'bold))
   (doom-modeline-project-dir        (:foreground base06 :weight 'bold))
   (doom-modeline-project-root-dir   (:foreground base06 :weight 'normal))
   (doom-modeline-project-parent-dir (:foreground base06 :weight 'normal))
   (doom-modeline-bar-inactive       (:foreground base04 :background base00))
   (doom-modeline-bar                (:background base01))
   (doom-modeline-evil-insert-state  (:foreground base06))
   (doom-modeline-evil-visual-state  (:foreground base06))
   (doom-modeline-evil-normal-state  (:foreground base06))
   (doom-modeline-evil-emacs-state   (:foreground base06))
   (doom-modeline-buffer-minor-mode  (:background base06))
   ;; Meow.
   (meow-beacon-indicator (:foreground base0A :background base0AS))
   (meow-insert-indicator (:foreground baseEX :background baseEXS))
   (meow-keypad-indicator (:foreground baseEX :background baseEXS))
   (meow-kmacro-indicator (:foreground base0A :background base0AS))
   (meow-motion-indicator (:foreground base0A :background base0AS))
   (meow-normal-indicator (:foreground base0B :background base0BS))
   ;; Orderless.
   (orderless-match-face-0 (:foreground base06 :bold t))
   (orderless-match-face-1 (:foreground base06 :bold t))
   (orderless-match-face-2 (:foreground base06 :bold t))
   (orderless-match-face-3 (:foreground base06 :bold t))
   ;; Terminal.
   (ansi-color-black          (:foreground base01))
   (ansi-color-red            (:foreground base0A))
   (ansi-color-green          (:foreground base0D))
   (ansi-color-yellow         (:foreground baseEX))
   (ansi-color-blue           (:foreground base0B))
   (ansi-color-magenta        (:foreground base0C))
   (ansi-color-cyan           (:foreground base08))
   (ansi-color-white          (:foreground base04))
   (ansi-color-bright-black   (:foreground base02 ))
   (ansi-color-bright-red     (:foreground base0A))
   (ansi-color-bright-green   (:foreground base0D))
   (ansi-color-bright-yellow  (:foreground baseEX))
   (ansi-color-bright-blue    (:foreground base0B))
   (ansi-color-bright-magenta (:foreground base0C))
   (ansi-color-bright-cyan    (:foreground base08))
   (ansi-color-bright-white   (:foreground base06))
   ;; Flyspell.
   (flyspell-incorrect       (:underline (:style 'wave :color base0A)))
   (flyspell-duplicate       (:underline (:style 'wave :color base0F)))
   (flycheck-error           (:underline (:style 'wave :color base0A)))
   (flysheck-warning         (:underline (:style 'wave :color base0F)))
   (flysheck-warning-overlay (:underline (:style 'wave :color base0F)))
   (flycheck-note            (:underline (:style 'wave :color base0D)))
   ;; Hydras.
   (hydra-face-red      (:foreground base06 :bold t))
   (hydra-face-blue     (:foreground base06 :bold t))
   (hydra-face-amaranth (:foreground base06 :bold t))
   (hydra-face-pink     (:foreground base06 :bold t))
   (hydra-face-teal     (:foreground base06 :bold t))
   ;; Cider.
   (cider-fringe-good-face      (:foreground base0D))
   (cider-test-error-face       (:background base0A))
   (cider-result-overlay-face   (:background base03 :box (:line-width 1 :color base05)))
   ;; Highlight Numbers
   (highlight-numbers-number    (:foreground base0F))
   ))

    ;; (custom-theme-set-variables 'test
    ;;     `(ansi-color-names-vector [,example-red
    ;;                               ,example-green
    ;;                               ,example-blue
    ;;                               ,example-purple
    ;;                               ,example-yellow
    ;;                               ,example-orange
    ;;                               ,example-cyan]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'oxocarbon)
(provide 'oxocarbon-theme)

;; Local Variables:
;; eval: (and (when (fboundp 'rainbow-mode) (rainbow-mode +1)) (when (fboundp 'rainbow-delimiters-mode) (rainbow-delimiters-mode +1)))
;; End:

;;; oxocarbon-theme.el ends here
