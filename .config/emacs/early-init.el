;;; early-init.el -*- lexical-binding: t; -*-
;;; Commentary: Where things loads before everything begins.

;;; Code:

;; UI options are set earlier.
(setq tool-bar-mode nil
      menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(tooltip-mode -1)
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(setq-default window-divider-default-right-width 24
              window-divider-default-bottom-width 12
              window-divider-default-places t
              left-margin-width 0
              right-margin-width 0
              window-combination-resize nil
              window-min-height 1)
(set-window-margins nil 1)
(window-divider-mode t)

(setq default-frame-alist '((min-height . 1)
                           '(height . 1)
                            (min-width  . 1)
                           '(width  . 81)
                            (vertical-scroll-bars . nil)
                            (internal-border-width . 24)
                            (left-fringe . 8)
                            (right-fringe . 1)
                            (tool-bar-lines . 0)
                            (menu-bar-lines . 0)))
(setq initial-frame-alist default-frame-alist)

;; Don't warn about these.
(setq warning-minimum-level :error)
(setq use-dialog-box nil)
(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)

;; PM initiliazed earlier.
(require 'seq)

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;###autoload
(defmacro elpaca-leaf (order &rest body)
  "Execute BODY in `leaf' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-leaf" order)
    (let ((o order))
      (when-let ((ensure (seq-position body :ensure)))
	(setq o (if (null (nth (1+ ensure) body)) nil order)
	      body (append (seq-subseq body 0 ensure)
			   (seq-subseq body (+ ensure 2)))))
      `(elpaca ,o (leaf
		   ,(if-let (((memq (car-safe order) '(quote \`)))
			     (feature (flatten-tree order)))
			(cadr feature)
		      (elpaca--first order))
		   ,@body)))))

(elpaca leaf (require 'leaf))
(elpaca leaf-keywords (require 'leaf-keywords))

(elpaca-leaf nil
	     :init (leaf leaf-keywords
			 :init (leaf-keywords-init)))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    :config
    (leaf-keywords-init)))

(leaf cl-lib
  :leaf-defer t
  :ensure t)

(elpaca-wait)

;; End:

;;; early-init.el ends here.
