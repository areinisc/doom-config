;;;  -*- lexical-binding: t; -*-


;; Set theme to doom-nova (from doom-themes)
(setq doom-theme 'doom-nova)

;;; quicklisp + SLIME + SBCL = LISP development
;;; define slime package as proscribed by hlissner:
;;; Option A)
(def-package! slime
  :defer t
  :init (load "slime-autoloads" nil t)
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy)))
;;; Option B)
;; (def-package! slime
;;   :commands (slime slime-mode slime-connect slime-selector slime-setup)
;;   :hook (lisp-mode . slime-lisp-mode-hook)
;;   :config
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;         slime-contribs '(slime-fancy)))
