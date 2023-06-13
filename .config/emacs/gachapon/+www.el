;;; -*- lexical-binding: t; -*-

;; HTML
(elpaca-leaf emmet-mode
  :hook ((sgml-mode-hook . emmet-mode)
         (css-mode-hook  . emmet-mode))
  :config
  (setq emmet-indent-after-insert nil))

(elpaca-leaf haml-mode)
(elpaca-leaf pug-mode)

(defun pug-compile-saved-file()
  (when (and (stringp buffer-file-name)
             (string-match "\\.pug\\'" buffer-file-name))
    (pug-compile))) ;; Requires `pug-cli' from NPM

(add-hook
 'after-save-hook
 'pug-compile-saved-file)

(elpaca-leaf slim-mode)

;; CSS
(leaf css-mode)
(elpaca-leaf less-css-mode)
(elpaca-leaf sass-mode)
(elpaca-leaf rainbow-mode)

;; JS
(elpaca-leaf add-node-modules-path)
(elpaca-leaf coffee-mode)
(elpaca-leaf js2-mode)
(elpaca-leaf js2-refactor)
(elpaca-leaf npm-mode)
(elpaca-leaf rjsx-mode)

(elpaca-leaf skewer-mode
  :hook ((css-mode-hook . skewer-css-mode)
         (html-mode-hook . skewer-html-mode)
         (js2-mode-hook . skewer-mode)))

(elpaca-leaf tide)
(elpaca-leaf xref-js2)

(provide '+www)

;; End:

;;; +www.el ends here
