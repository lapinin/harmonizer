;;; -*- lexical-binding: t; -*-

;; CC
(leaf cc-vars
  :leaf-defer t
  :config
  (setq-default c-basic-offset 4))
  (setq c-default-style '((java-mode  "java")
                          (awk-mode  "awk")))

(elpaca-leaf modern-cpp-font-lock
  :diminish
  :hook c++-mode modern-c++-font-lock-mode)

(provide '+metal)

;; End:

;;; +metal.el ends here
