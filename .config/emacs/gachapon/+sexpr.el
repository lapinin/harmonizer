;;; -*- lexical-binding: t; -*-

;; CL
(add-to-list 'exec-path "/usr/local/bin")
(setq inferior-lisp-program "sbcl")

(leaf lisp-mode
  :mode "\\.eclrc$"
  :mode "\\.ros$"
  :mode "\\.sbclrc$"
  :mode "\\.slynkrc$"
  :mode "\\.lispworks$")

(elpaca-leaf sly
  :leaf-defer t)

(elpaca-leaf sly-asdf
  :after sly
  :bind ("C-c L" . popn/load-current-system-or-ask)
  :leaf-defer nil
  :config
  (defun popn/load-current-system-or-ask ()
    (interactive)
    (if (sly-connected-p)
        (sly-asdf-load-system (or (sly-asdf-find-current-system) (sly-asdf-read-system-name)))
      (message "Not connected."))))

(elpaca-leaf sly-macrostep
  :after sly)

(elpaca-leaf sly-named-readtables
  :after sly)

(elpaca-leaf sly-repl-ansi-color
  :after sly
  :config (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(when (and (featurep 'window-purpose)
           (featurep 'sly))
  (purpose-set-extension-configuration
   :sly
   (purpose-conf :name-purposes '(("*sly-macroexpansion*" . search)
                                  ("*sly-description*" . search)
                                  (" *sly-completion doc*" . search)
                                  ("*sly-compilation*" . search))
                 :regexp-purposes '(("\\*sly-db.\**" . search)
                                    ("\\*sly-xref.\**" . search))
                 :mode-purposes '((sly-mrepl-mode . terminal)
                                  (sly-inspector-mode . search))))

  (defun popn/sly-load-layout ()
    (let ((layout (purpose-find-window-layout "sly")))
      (when layout
        (purpose-load-window-layout-file layout))))
  (add-hook 'sly-connected-hook 'popn/sly-load-layout t)

  (defun popn/sly-reset-layout (_)
    (purpose-load-recent-window-layout 1))
  (add-hook 'sly-net-process-close-hooks 'popn/sly-reset-layout t)

  (defun popn/sly-inspector-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-inspector-mode-hook 'popn/sly-inspector-mode-hook)

  (defun popn/sly-mrepl-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-mrepl-mode-hook 'popn/sly-mrepl-mode-hook)

  (defun popn/sly-db-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-db-mode-hook 'popn/sly-db-mode-hook))

;; CLJ
(elpaca-leaf flycheck-clj-kondo)

(elpaca-leaf clojure-mode
  :bind (("C-c C-v" . cider-start-http-server)
         ("C-M-r" . cider-refresh)
         ("C-c u" . cider-user-ns))
  :config
  (require 'flycheck-clj-kondo))

(elpaca-leaf clojure-mode-extra-font-locking)

(elpaca-leaf cider
  :bind ("C-c u" . cider-user-ns)
  :config
  (setq cider-repl-pop-to-buffer-on-connect t)      ; Go right to the REPL buffer when it's finished connecting
  (setq cider-show-error-buffer nil)                ; When there's a cider error, show its buffer and switch to it
  (setq cider-repl-display-help-banner nil))        ; Help banner 

(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; ELISP
(elpaca-leaf fn)      ; function
(elpaca-leaf s)       ; string
(elpaca-leaf f)       ; file
(elpaca-leaf ht)      ; hash table
(elpaca-leaf dash)    ; list
(elpaca-leaf a)       ; assoc lists
(elpaca-leaf async)   ; async
(elpaca-leaf ts)      ; timestamps
(elpaca-leaf pcre2el) ; sane regex
(elpaca-leaf hydra)   ; cool hydras :p

;; FENNEL
(elpaca-leaf lua-mode
  :leaf-defer t
  :init
  ;; lua-indent-level defaults to 3 otherwise. Madness.
  (setq lua-indent-level 2)
  :hook lua-mode-hook antifennel-mode)

(elpaca-leaf fennel-mode
  :hook fennel-mode-hook fennel-proto-repl-minor-mode) ;; https://andreyor.st/posts/2023-04-08-new-fennel-proto-repl-and-call-for-testing/

;; HY
(elpaca-leaf hy-mode)

(provide '+sexpr)

;; End:

;;; +sexpr.el ends here
