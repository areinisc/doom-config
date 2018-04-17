;;;  -*- lexical-binding: t; -*-

;;; THEME

;; When not on mac, set theme to doom-nova (from doom-themes)
(unless (eq system-type 'darwin)
  (setq doom-theme 'doom-nova))

;; When on mac, use brightness sensor for theme adjustment
(when (eq system-type 'darwin)
  ;; Set your light and dark theme choices here!
  (defconst light-theme 'doom-solarized-light)
  (defconst dark-theme 'doom-nova)

  (setq doom-theme 'doom-nova ; starting (dark) theme
        theme-is-dark t)      ; boolean to track if theme is dark

  ;; Set 2nd argument to how often (in seconds) you want to check light sensor.
  (run-with-timer 10 10 #'change-theme-for-lighting))

(defun change-theme-for-lighting ()
  "Function for sensing light and changing themes."
  (let* ((current-light-sensor-reading
          (string-to-number
           (shell-command-to-string
            ;; this assumes you put the `lmutracker` executable
            ;; in your doom-config directory ("~/.doom.d/" or "~/.config/doom/")
            (concat doom-private-dir "lmutracker")))))
    (if (< current-light-sensor-reading 100000) ; test if environment is low-light
        (unless theme-is-dark         ; if theme is not yet dark
          (load-theme dark-theme 1)   ; load dark theme
          (setq theme-is-dark t))     ; note that theme is dark now

      (when theme-is-dark             ; if theme is dark
        (load-theme light-theme 1)    ; load light theme
        (setq theme-is-dark nil)))))  ; note that theme is NOT dark now


;;; PLUGINS

;; SLIME
;; quicklisp + SLIME + SBCL = LISP development
;; define slime package as proscribed by hlissner:
;; Option A)
(def-package! slime
  :defer t
  :init (load "slime-autoloads" nil t)
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/local/bin/sbcl"
        slime-contribs '(slime-fancy)))
;; Option B)
;; (def-package! slime
;;   :commands (slime slime-mode slime-connect slime-selector slime-setup)
;;   :hook (lisp-mode . slime-lisp-mode-hook)
;;   :config
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;         slime-contribs '(slime-fancy)))

;; paredit
(def-package! paredit
  :commands (paredit-mode enable-paredit-mode)
  :hook (lisp-mode . enable-paredit-mode))
;; Option B)
;; Doesn't enforce paredit mode for lisp files, just autoloads it.
;; (def-package! paredit
;;   :defer t
;;   :init (load "paredit-autoloads" nil t))
