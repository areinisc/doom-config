;;;  -*- lexical-binding: t; -*-


;; Set theme to doom-nova (from doom-themes)
(setq doom-theme 'doom-nova)

;;; quicklisp + SLIME + SBCL = LISP development
;; define slime package
(def-package! slime
  :commands slime
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;; ;; I did these here before trying to add them to def-package!
;; ;; set up quicklisp
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
;; (setq slime-contribs '(slime-fancy))

; (load (expand-file-name "~/quicklisp/slime-helper.el"))
