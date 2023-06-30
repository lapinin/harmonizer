;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Where everything begins.

;;; Code:

(require 'popn
  (concat user-emacs-directory
          "popn/popn"))

(popn! +modal
       +completions
       +dired
       +docs
       +sexpr
       +www
       +fp
       +metal
       +provers
       +status)

;; End:

;;; init.el ends here.
