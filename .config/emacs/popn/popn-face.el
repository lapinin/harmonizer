;;; popn-face.el -*- lexical-binding: t; -*-
;;; Commentary: Faces and colors.

;;; Code:

(require 'popn-settings)
(require 'popn-core)

(defcustom popn-font-size 12
  "Default value for the font size in pt units."
  :group 'popn
  :type 'integer)

(defcustom popn-font "monospace"
  "Default font."
  :group 'popn
  :type 'string)

(defcustom popn-font-unicode nil
  "Default unicode font."
  :group 'popn
  :type 'string)

(defcustom popn-variable-pitch-font nil
  "Default variable pitch font."
  :group 'popn
  :type 'string)

(defcustom popn-chinese-font nil
  "Default font for Chinese glyphs."
  :group 'popn
  :type 'string)

(defcustom popn-japanese-font nil
  "Default font for Japanese glyphs." 
 :group 'popn
  :type 'string)

(defcustom popn-korean-font nil
  "Default font for Korean glyphs."
  :group 'popn
  :type 'string)

(setq
 popn-font-size 10
 popn-font "Martian Mono Cn Md 1.4"
 popn-font-unicode "JuliaMono"
 popn-variable-pitch-font "IBM Plex Sans"
 popn-chinese-font "Source Han Serif CN"
 popn-japanese-font "IBM Plex Sans JP"
 popn-korean-font "IBM Plex Sans KR")

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
                    :height 140)

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
                    (font-spec :family popn-korean-font :height 120))
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

;; Theming.

(elpaca-leaf autothemer
  :ensure t)

(setq custom-safe-themes t)
(setq custom--inhibit-theme-enable nil)
(leaf oxocarbon-theme
  :config
  (require 'autothemer)
  (require 'dash)
  (load-theme 'oxocarbon t))

(elpaca-leaf rainbow-delimiters
  :require t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'fennel-mode-hook #'rainbow-delimiters-mode))

(elpaca-leaf highlight-numbers
  :require t
  :config
  (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")
  (add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (add-hook 'clojure-mode-hook #'highlight-numbers-mode)
  (add-hook 'emacs-mode-hook #'highlight-numbers-mode)
  (add-hook 'lisp-mode-hook #'highlight-numbers-mode)
  (add-hook 'fennel-mode-hook #'highlight-numbers-mode))

(elpaca-leaf doom-modeline
  :require t
  :config
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-modal nil)
  (doom-modeline-mode))

(elpaca-leaf (ws-butler :host github
                        :repo "hlissner/ws-butler")
  :config
  (setq ws-butler-keep-whitespace-before-point nil))

(leaf paren
  :ensure t
  :config
  (setq show-paren-style 'mixed)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

(elpaca-leaf (ct.el :host github
                    :repo "neeasade/ct.el"))

(elpaca-leaf focus)

(provide 'popn-face)

;; End:

;;; popn-face.el ends here.
