;;; popn.el -*- lexical-binding: t; -*-
;;; Commentary: Main file.

;;; Code:

;; Ensure `popn' is in `load-path'
(add-to-list
 'load-path (file-name-directory load-file-name))

(require 'popn-core)

;; Security settings
;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv-internal "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
  ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
  ;; used in that case. Otherwise, people have reasons to not go with
  ;; `gnutls', we use `openssl' instead. For more details, see
  ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Don't load any other files besides this config
(setq inhibit-default-init t)

(require 'popn-pkg)

;; Define directories
(eval-and-compile
  (defvar home-directory (getenv "HOME")
    "User $HOME.")

  (defvar popn-root (file-truename user-emacs-directory)
    "Root of popn.")

  (defvar popn-dir (concat popn-root "popn/")
    "The main directory of popn's configuration."))

;; Add configuration directories to `load-path'
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("popn" "gachapon"))
    (cl-pushnew (expand-file-name dir popn-root) load-path)))

(update-load-path)

;; Remove command line options that aren't relevant to our current OS
(unless *IS-MAC*   (setq command-line-ns-option-alist nil))
(unless *IS-LINUX* (setq command-line-x-option-alist nil))

;; ポップン!
(defmacro popn! (&rest body)
  (declare (indent defun))
  (let ((gc-cons-threshold most-positive-fixnum))
    (add-to-list 'body 'env-fun t)
    (dolist (pkg body)
      (require pkg nil t))))

(require 'popn-face)
(require 'popn-settings)
(require 'popn-env)

(provide 'popn)

;; End:

;;; popn.el ends here.
