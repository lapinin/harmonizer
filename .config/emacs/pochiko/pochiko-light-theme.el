;;; pochiko-theme.el --- A theme. -*- lexical-binding: t -*-

;; Author: Liz Adoo <>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces
;; Homepage: 

;;; Commentary: Work in progress non-functional atm.
;;; Code:

(deftheme pochiko
  "A theme.")

(defgroup pochiko nil
  "Pochiko theme properties."
  :group 'faces)

(let ((class '((class color) (min-colors 89)))
      (main-bg "#ffffff") (main-fg "#000000")
      (red "#a00000") (green "#005000") (blue "#000077"))
  (custom-theme-set-faces
   'pochiko-light
   `(default ((,class :background ,main-bg :foreground ,main-fg)))
   `(cursor ((,class :background ,red)))
   `(font-lock-builtin-face ((,class :foreground ,blue))))
   `(font-lock-string-face ((,class :foreground ,green))))


(provide-theme 'pochiko-light)
(provide 'pochiko-light-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; pochiko-theme.el ends here.
