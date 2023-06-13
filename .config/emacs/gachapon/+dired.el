;;; -*- lexical-binding: t; -*-

(leaf dired
  :require t
  :config
  (setq dired-hide-details-hide-symlink-targets nil
        dired-omit-verbose nil
        dired-listing-switches "-AFhlv --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies 'always))

(leaf find-dired
  :after dired
  :config
  (setq find-ls-option
        '("-ls" . "-AFhlv --group-directories-first")
        find-name-arg "-iname"))

(leaf diredfl
  :hook dired-mode diredfl-mode)

(leaf dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil
        dired-subtree-line-prefix "  "))

(elpaca-leaf (dired-async :host github
                          :repo "jwiegley/emacs-async"
                          :files (:defaults "dired-async.el"))
  :after (dired async)
  :hook dired-mode dired-async-mode)

(provide '+dired)

;; End:

;;; +dired.el ends here
