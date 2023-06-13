;;; -*- lexical-binding: t; -*-

;; WIP ...

;; ;; Agda
;; (load-file (let ((coding-system-for-read 'utf-8))
;;              (shell-command-to-string "agda-mode locate")))

;; ;; Coq
;; (p! proof-general)

;; ;; Lean
;; (p! lean4-mode
;;   :straight (lean4-mode :type git :host github :repo "leanprover/lean4-mode")
;;   :commands (lean4-mode))

(provide '+provers)

;; End:

;;; +provers.el ends here
