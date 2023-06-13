;;; popn-settings.el -*- lexical-binding: t; -*-
;;; Commentary: Popn's settings.

;;; Code:

(defgroup popn nil
  "Popn-emacs custom options."
  :group 'emacs)

(defvar popn-user-shell 'hilbish
  "Define user shell, e.g. bash, zsh, etc.")

(defcustom popn-default-indent-width 4
  "Set default indentation width. By default is 4."
  :group 'popn)

;; Enable mouse within terminal.
(when (not window-system)
  (xterm-mouse-mode t))

;; Replacing yes/no with y/n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set tab length.
(setq tab-stop-list (number-sequence 2 120 2))

;; Allow commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Use hilbish.
(setq shell-file-name "/usr/local/bin/hilbish")

(elpaca-leaf vterm
  :ensure t
  :config
  (setq vterm-disable-inverse-video t
        vterm-disable-underline t
        vterm-ignore-blink-cursor t
        vterm-kill-buffer-on-exit t
        vterm-max-scrollback 4000
        vterm-set-bold-hightbright nil
        vterm-term-environment-variable "xterm-mono"
        vterm-timer-delay 0.1))

;; Kill term buffer when exiting.
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; Native-comp setting for Emacs 28+.
(if (and (not (version< emacs-version "28.0")) (featurep 'nativecomp))
    (leaf comp
      :config
      (setq-default comp-async-compilation t
                    ;; native-comp-deferred-compilation nil
                    ;; Disable warning
                    native-comp-async-report-warnings-errors nil)))

;; Suppress compile messages.
(when (and (version< emacs-version "28.0") (featurep 'nativecomp)
           (define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
             (let ((obsolete-name (pop ll))
                   (current-name (pop ll))
                   (when (if ll (pop ll) "1"))
                   (docstring (if ll (pop ll) nil)))
               (list obsolete-name current-name when docstring)))))

(elpaca-leaf gcmh
  :ensure t
  :config
  (setq gcmh-verbose nil
        gcmh-idle-delay 5
        inhibit-compacting-font-caches t
        gc-cons-percentage 0.1))

(leaf delsel
  :require t
  :init (delete-selection-mode))

(leaf simple
  :require t
  :config
  (setq-default yank-pop-change-selection t
                suggest-key-bindings t
                kill-whole-line t
                eval-expression-print-level nil
                set-mark-command-repeat-pop t
                async-shell-command-buffer 'new-buffer
                backward-delete-char-untabify-method 'hungry
                track-eol t
                line-move-visual nil
                idle-update-delay 1.0
                jit-lock-defer-time 0))

(leaf autorevert
  :init 
  (global-auto-revert-mode)
  :config
  (setq auto-revert-interval 10
        auto-revert-check-vc-info nil
        auto-revert-use-notify nil
        auto-revert-verbose nil))

(leaf files
  :require t
  :config
  (setq make-backup-files nil
        backup-by-copying t
        backup-by-copying-when-linked t
        backup-directory-alist '(("." . "~/.cache/emacs/backup"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        auto-save-default nil
        auto-save-list-file-prefix nil
        large-file-warning-threshold nil
        require-final-newline t
        find-file-visit-truename t
        confirm-kill-processes nil
        confirm-kill-emacs 'y-or-n-p
        auto-mode-case-fold nil))

(leaf find-file
  :require t
  :leaf-defer t
  :config
  (setq ff-quiet-mode t))

(leaf compile
  :config
  (setq compilation-always-kill t
        compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-skip-threshold 2))

;; Various Emacs settings...
;; Set variables defined in C source code.
(leaf emacs
  :require t
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; TAB cycle if there are only few candidates.
  (setq completion-cycle-threshold 3)

  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  :hook eval-expression-minibuffer-setup show-paren-mode
  :config
  (setq inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-echo-area-message t
        initial-scratch-message nil
        initial-buffer-choice nil
        frame-title-format nil
        use-dialog-box nil
        pop-up-windows nil
        cursor-in-non-selected-windows nil
        frame-resize-pixelwise t
        visible-bell nil
        ring-bell-function 'ignore
        frame-title-format nil
        default-directory "~/"
        scroll-margin 0 
        hscroll-margin 0
        scroll-conservatively 10000
        auto-window-vscroll nil
        mac-mouse-wheel-smooth-scroll nil
        pixel-scroll-precision-mode t
        use-file-dialog nil
        use-dialog-box nil
        x-gtk-use-system-tooltips nil
        x-underline-at-descent-line t
        create-lockfiles nil
        mode-line-default-help-echo nil)
  (setq-default tab-width popn-default-indent-width
                indent-tabs-mode nil
                line-spacing 1
                fill-column 80
                x-stretch-cursor nil
                visible-cursor nil
                highlight-nonselected-windows nil
                bidi-display-reordering nil
                indicate-buffer-boundaries nil
                indicate-empty-lines nil
                truncate-lines t
                truncate-partial-width-windows nil
                mouse-yank-at-point t
                apropos-do-all t))

(provide 'popn-settings)

;; End:

;;; popn-settings.el ends here.
