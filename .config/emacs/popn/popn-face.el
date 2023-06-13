;;; popn-face.el -*- lexical-binding: t; -*-
;;; Commentary: Faces and colors.

;;; Code:

(require 'popn-settings)
(require 'popn-core)

(defcustom popn-font-size 14
  "Default value for the font size in pt units."
  :group 'popn
  :type 'integer)

(defcustom popn-font "Triplicate T4c"
  "Default font."
  :group 'popn
  :type 'string)

(defcustom popn-font-unicode "JuliaMono"
  "Default unicode font."
  :group 'popn
  :type 'string)

(defcustom popn-variable-pitch-font "Equity Text A"
  "Default variable pitch font."
  :group 'popn
  :type 'string)

(defcustom popn-chinese-font "Source Han Serif SC VF"
  "Default font for Chinese glyphs."
  :group 'popn
  :type 'string)

(defcustom popn-japanese-font "Source Han Serif JP VF"
  "Default font for Japanese glyphs." 
 :group 'popn
  :type 'string)

(defcustom popn-korean-font "Source Han Serif KR VF"
  "Default font for Korean glyphs."
  :group 'popn
  :type 'string)

(setq
  popn-font-size 16
  popn-font "Fairfax"
  popn-font-unicode "Fairfax")
  
(defun what-faces? (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(set-face-attribute 'default nil
                    :font (font-spec :antialias t)
                    :family popn-font
                    :height (* popn-font-size 10)
                    :weight 'regular)
  
(set-face-attribute 'variable-pitch nil
                    :family popn-japanese-font
                    :height (* popn-font-size 10)
                    :weight 'regular)

(set-face-attribute 'fixed-pitch nil
                    :family popn-variable-pitch-font
                    :weight 'regular)

(set-face-attribute 'variable-pitch-text nil
                    :family popn-variable-pitch-font
                    :height 120)

(when (fboundp #'set-fontset-font)
  (set-fontset-font t 'unicode
                    (font-spec :name popn-font-unicode))
  (set-fontset-font t 'han
                    (font-spec :family popn-chinese-font))
  (set-fontset-font t 'japanese-jisx0213.2004-1
                    (font-spec :family popn-japanese-font))
  (set-fontset-font t 'kana
                    (font-spec :family popn-japanese-font))
  (set-fontset-font t 'hangul
                    (font-spec :family popn-korean-font))
  (set-fontset-font t 'cjk-misc
                    (font-spec :family popn-japanese-font)))

;; Fallback font for glyphs.
(defface fallback '((t :family popn-font-unicode
                       :inherit 'default))
  "Fallback")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                         (make-glyph-code ?↩ 'fallback))

;; Theme.
(elpaca-leaf stimmung-themes
  :ensure t
  :config (stimmung-themes-load-light))

(elpaca-leaf circadian
  :ensure t
  :config
  (setq circadian-themes '(("6:00" . stimmung-themes-light)
                           ("17:30" . stimmung-themes-dark)))
  (circadian-setup))

(elpaca-leaf aas)
(elpaca-leaf laas)
(elpaca-leaf engrave-faces)
(elpaca-leaf info-colors)

(elpaca-leaf dimmer
  :require t
  :config
  (setq dimmer-fraction 0.8
        dimmer-adjustment-mode :foreground
        dimmer-use-colorspace :rgb
        dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-mode))

(elpaca-leaf focus
  :require t
  :config
  (focus-mode))

  ;; :config
  ;; ;; add whatever lsp servers you use to this list
  ;; (add-list-to-list 'focus-mode-to-thing
  ;;                   '((lua-mode . lsp-folding-range)
  ;;                     (rust-mode . lsp-folding-range)
  ;;                     (latex-mode . lsp-folding-range)
  ;;                     (python-mode . lsp-folding-range))))

(elpaca-leaf doom-modeline
  :require t
  :config
  (setq doom-modeline-height 14)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-modal nil)
  (doom-modeline-mode))

(elpaca-leaf (screenshot :host github
                         :repo "tecosaur/screenshot"))

(elpaca-leaf (ws-butler :host github
                        :repo "hlissner/ws-butler")
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(elpaca-leaf rainbow-delimiters
  :hook prog-mode rainbow-delimiters-mode)

(elpaca-leaf (zen-mode :host github
                       :repo "aki237/zen-mode"))

(leaf paren
  :ensure t
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(elpaca-leaf goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(provide 'popn-face)

;; End:

;;; popn-face.el ends here.
