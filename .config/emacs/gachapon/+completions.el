;;; -*- lexical-binding: t; -*-

(require 'popn-core)

(elpaca-leaf vertico
  :config
  (vertico-mode)
  (setq vertico-count 4)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  (setq vertico-mouse-mode t))

(advice-add #'vertico--format-candidate :around
            (lambda (orig cand prefix suffix index _start)
              (setq cand (funcall orig cand prefix suffix index _start))
              (concat
               (if (= vertico--index index)
                   (propertize " >> " 'face 'vertico-current)
                 "  ")
               cand)))

(leaf savehist
  :config
  (savehist-mode))

(elpaca-leaf orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(elpaca-leaf marginalia
  :bind ("M-A" . marginalia-cycle)
  :init
  (marginalia-mode))

(elpaca-leaf lexic)

(elpaca-leaf flycheck
  :hook
  ((prog-mode . flycheck-mode)
   (emacs-lisp-mode . (lambda () (flycheck-mode -1)))))

(leaf flyspell)

(leaf ispell
  :config
  (setq ispell-dictionary "english")
  (setq ispell-silently-savep t))

(elpaca-leaf magit)

(elpaca-leaf yasnippet
  :config
  (yas-global-mode 1))

(elpaca-leaf markdown-mode)

(provide '+completions)

;; End:

;;; +completions.el ends here
