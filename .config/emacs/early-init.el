;; -*- lexical-binding: t; -*-
;;; early-init.el
;;; Commentary: Where things loads before everything begins.
;;; Code:

(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(setq-default window-divider-default-right-width 24
              window-divider-default-places 'right-only
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil
              window-min-height 1)
(window-divider-mode t)

(setq default-frame-alist '((min-height . 1)  '(height . 45)
                            (min-width  . 1)  '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 0)
                            (right-fringe . 0)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))
(setq initial-frame-alist default-frame-alist)

(global-font-lock-mode nil)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)

;;; early-init.el ends here.
