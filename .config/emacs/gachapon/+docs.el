;;; -*- lexical-binding: t; -*-

(elpaca-leaf doct)
(elpaca-leaf websocket)

;; ORG
(elpaca-leaf org
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :init
  (setq org-catch-invisible-edits 'smart
        org-log-done 'time
        org-list-allow-alphabetical t
        org-export-in-background t
        org-ellipsis " ↓"
        org-hide-emphasis-markers t
        org-indent-indentation-per-level 1
        org-return-follows-link t
        org-special-ctrl-a/e t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-support-shift-select t
        org-adapt-indentation nil
        org-latex-create-formula-image-program 'dvipng)
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
  (setq org-format-latex-options
      (plist-put org-format-latex-options :scale 4.0))
  :config
  (define-key org-mode-map "\C-a" 'org-beginning-of-line)
  (define-key org-mode-map "\C-e" 'org-end-of-line))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (clojure . t)
   (haskell . t)
   (css . t)
   (emacs-lisp . t)
   (js . t)
   (lisp . t)))

(push
 '("conf-unix" . conf-unix)
 org-src-lang-modes)

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

(elpaca-leaf org-modern
  :hook (org-mode . global-org-modern-mode))

;; LATEX
;; TODO ... pending setup
(elpaca-leaf auctex)

(provide '+docs)

;; End:

;;; +docs.el ends here
