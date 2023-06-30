;;; early-init.el -*- lexical-binding: t; -*-
;;; Commentary: Where things loads before everything begins.

;;; Code:

;; UI options are set earlier.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(fringe-mode 12)

(tooltip-mode -1)
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(setq-default window-divider-default-right-width 24
              window-divider-default-bottom-width 12
              window-divider-default-places t
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil
              window-min-height 1)
(set-window-margins nil 4)
(window-divider-mode t)

(setq default-frame-alist '((min-height . 1)
                           '(height . 1)
                            (min-width  . 1)
                           '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 8)
                            (right-fringe . 1)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))
(setq initial-frame-alist default-frame-alist)

;; Don't warn about these.
(setq warning-minimum-level :error)
(setq use-dialog-box nil)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
;;(advice-add #'x-apply-session-resources :override #'ignore)

;; End:

;;; early-init.el ends here.
