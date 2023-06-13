;;; -*- lexical-binding: t; -*-

(elpaca-leaf (friar
              :host github
              :repo "warreq/friar"
              :files (:defaults "*.lua" "*.fnl"))
  :config
  (setq friar-fennel-file-path (concat home-directory "/.local/bin/fennel")))

(provide '+awm)

;;; End:

;;; +awm.el ends here.
