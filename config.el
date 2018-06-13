;;;  -*- lexical-binding: t; -*-


;;;
;;; FIXUP for macos
;;;

;;; macos title-bar fix
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))


;;;
;;; THEME
;;;

;; Set light and dark theme choices here!
(defconst light-theme 'doom-nord-light)
(defconst dark-theme 'doom-nord)
(defconst mac-default-theme dark-theme
  "Controls whether default theme is dark or light.")

;; photometry
;; I want to be able to toggle "photometry" (automatic theme switching),
;; but I don't know how to properly build a module or code in elisp.

;; Define integer variable to track photometry state (on/off; 1/0)
(defvar photometry-mode 0
  "photometry is recorded as *off*")

(defun photometry ()
  "Function for sensing light and changing themes based on apparent brightness
as reported through lmutracker executable. Adjust the integer compared to
current-light-sensor-reading to change low-light threshold---100000 means it's
fairly dark before switching to dark, higher numbers let you keep a dark theme
even with moderate ambient lighting."
  (let* ((current-light-sensor-reading
          (string-to-number
           (shell-command-to-string
            ;; this assumes you put the `lmutracker` executable
            ;; in your doom-config directory ("~/.doom.d/" or "~/.config/doom/")
            (concat doom-private-dir "lmutracker")))))
    (if (< current-light-sensor-reading 250000) ; test if environment is low-light
        (unless (eq doom-theme dark-theme) ; if theme is not yet dark
          (setq doom-theme dark-theme)     ; change to dark theme
          (doom//reload-theme))
      (when (eq doom-theme dark-theme)     ; if theme is dark
        (setq doom-theme light-theme)      ; change to light theme
        (doom//reload-theme)))))

(defun photometry/toggle ()
  "Toggle photometry on/off. Photometry is a function that changes the theme
over time based on ambient light sensor readings."
  (interactive)
  (if (zerop photometry-mode)
      (and (setq photometry-mode (1+ photometry-mode))
           (run-with-timer 0 10 #'photometry)) ; integer controls the update interval
    (and (setq photometry-mode (1- photometry-mode))
         (cancel-function-timers 'photometry))))

;; Add keybind so photometry can be toggled with `SPC t p`
(map! (:leader
        (:desc "toggle" :prefix "t"
          :desc "Photometry" :n "p" #'photometry/toggle)))

;; When on mac, use photometry for automatic theme adjustment
(when (eq system-type 'darwin)
  (setq doom-theme mac-default-theme) ; starting (dark) theme
  (photometry/toggle)          ; start with photometry on
  )

;; When not on mac, set theme to doom-nova (from doom-themes)
(unless (eq system-type 'darwin)
  (setq doom-theme 'doom-nova))


;;;
;;; PLUGINS
;;;

;; SLIME
;; quicklisp + SLIME + SBCL = LISP development
;; define slime package as proscribed by hlissner:
;; Option A)
(def-package! slime
  :defer t
  :init (load "slime-autoloads" nil t)
  :config
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
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
;; (def-package! paredit
;;   :commands (paredit-mode enable-paredit-mode)
;;   :hook (lisp-mode . enable-paredit-mode))
;; Option B)
;; Doesn't enforce paredit mode for lisp files, just autoloads it.
;; (def-package! paredit
;;   :defer t
;;   :init (load "paredit-autoloads" nil t))

; ;; ein
; (after! ein
;         (set! :ein-notebook-dir "~/Documents"))

;;;
;;; KEYBINDS
;;;

;; smartparens
;; (map!
;;   (:after smartparens
;;     (:map smartparens-mode-map
;;       ",s" #'sp-splice-sexp
;;       ",f" #'sp-forward-slurp-sexp
;;       ",w" #'sp-wrap-round)))
(map!
 (:map lisp-mode-shared-map
   (:prefix "C-,"
     "s"  #'sp-splice-sexp
     "w"  #'sp-wrap-round
     "f"  #'sp-forward-slurp-sexp
     "b"  #'sp-forward-barf-sexp)))


;; (after! magit
;;   (setq magit-display-buffer-function #'magit-display-buffer-traditional))
