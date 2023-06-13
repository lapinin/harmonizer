;;; -*- lexical-binding: t; -*-

;; HASKELL
(elpaca-leaf haskell-mode
  :hook ((haskell-mode . haskell-indent-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . (lambda () (electric-indent-mode -1))))
  :bind (("C-c C-l" . haskell-process-load-or-reload))
  :config
  (setq haskell-process-type 'cabal-repl)
  (setq haskell-process-log t))
  ;;(haskell-stylish-on-save t))

;; PURS
(elpaca-leaf (purescript-mode :host github
                              :repo "purescript-emacs/purescript-mode")
  :mode (("\\.purs\\'" . purescript-mode)))

;; NIX
(elpaca-leaf nix-mode
  :mode "\\.nix\\'")

(provide '+fp)

;; End:

;;; +fp.el ends here
