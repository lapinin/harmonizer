;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Where everything begins.

;;; Code:

(require 'popn
  (concat user-emacs-directory
          "popn/popn"))

(popn! +modal
       +completions
       +dired
       +ibuffer
       +docs
       +sexpr
       +www
       +fp
       +metal
       ;; +provers
       +awm)

(custom-set-variables
 '(package-selected-packages '(paren cl-lib hydra leaf-keywords leaf)))

;; End:

;;; init.el ends here.
