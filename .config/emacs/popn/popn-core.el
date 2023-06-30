;;; popn-core.el -*- lexical-binding: t; -*-
;;; Commentary: Core settings.

;;; Code:

;; Detect system.
(defconst *IS-LINUX*   (eq system-type 'gnu/linux))
(defconst *IS-MAC*     (eq system-type 'darwin))
(defconst *IS-WINDOWS* (memq system-type '(cygwin windows-nt ms-dos)))
(defconst *IS-BSD*     (or *IS-MAC* (eq system-type 'berkeley-unix)))

;; Detect SVG format.
(add-to-list
 'image-types 'svg)

(provide 'popn-core)

;; End:

;;; popn-core.el ends here.
