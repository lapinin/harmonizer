;;; -*- lexical-binding: t; -*-

(leaf ibuffer
  :hook ibuffer-mode isettings
  :config
  (setq ibuffer-saved-filter-groups
      (quote (("main"
               ("MODIFIED" (and (modified . t)
                                (visiting-file . t)))
               ("term" (or (derived-mode . comint-mode)
                           (mode . vterm-mode)
                           (mode . eshell-mode)
                           (mode . term-mode)
                           (mode . shell-mode)))
               ("planning" (or (name . "^\\*Calendar\\*$")
                               (name . "^diary$")
                               (mode . org-agenda-mode)))
               ("blog" (filename . "/sites/personal-site/"))
               ("browser" (or (mode . xwidget-webkit-mode)
                              (mode . eww-mode)))
               ("notes" (and (filename . "/Notes/")
                             (or (mode . org-mode)
                                 (mode . markdown-mode))))
               ("org" (mode . org-mode))
               ("books" (filename . "/Books/"))
               ("docs" (or (mode . pdf-view-mode)
                           (mode . doc-view-mode)))
               ("img" (mode . image-mode))
               ("elisp" (or (filename . "/.emacs.d/")
                            (filename . "/.config/emacs/")
                            (mode . Custom-mode)))
               ("config" (or (filename . "/.config/")
                             (filename . "/.themes/")))
               ("code" (or (derived-mode . prog-mode)
                           (mode . ess-mode)
                           (filename . "/projects/")))
               ("dired" (or (mode . dired-mode)
                            (mode . dirvish-mode)))
               ("chat" (or (mode . ement-room-list-mode)
                           (mode . ement-room-mode)
                           (mode . erc-mode)
                           (mode . rcirc-mode)))
               ("help" (or (name . "\*Help\*")
                           (name . "\*Apropos\*")
                           (name . "\*info\*")
                           (mode . help-mode)))
               ("internal" (name . "^\*.*$"))
               ("other" (name . "^.*$"))))))
  (setq ibuffer-expert t
        ibuffer-show-empty-filter-groups nil
        ibuffer-use-header-line t
        ibuffer-always-show-last-buffer t))

(defun isettings ()
  (ibuffer-auto-mode 1)
  (ibuffer-switch-to-saved-filter-groups "main"))

(define-ibuffer-column name-nolabel
  (:name "Buffer"
         :inline nil
         :summarizer
         (lambda (strings)
           (let ((bufs (length strings)))
             (cond ((zerop bufs) "No buffers")
                   ((= 1 bufs) "1 buffer")
                   (t (format "%s buffers" bufs))))))
  (let ((string (propertize (buffer-name)
                            'font-lock-face
                            (ibuffer-buffer-name-face buffer mark))))
    (if (not (seq-position string ?\n))
        string
      (string-replace
       "\n" (propertize "^J" 'font-lock-face 'escape-glyph) string))))

(define-ibuffer-column size-custom
  (:name "Size"
         :inline t
         :summarizer
         (lambda (column-strings)
           (let ((total 0))
             (dolist (string column-strings)
               (setq total
                     (+ (float (my/human-readable-file-sizes-to-bytes string))
                        total)))
             (file-size-human-readable total))))
  (file-size-human-readable (buffer-size)))

(defun my/human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes."
  (interactive)
  (cond ((string-suffix-p "G" string t)
         (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
        ((string-suffix-p "M" string t)
         (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
        ((string-suffix-p "K" string t)
         (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
        (t
         (string-to-number (substring string 0 (- (length string) 1))))))


(provide '+ibuffer)

;; End:

;;; +ibuffer.el ends here
