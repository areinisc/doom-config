;;;  -*- lexical-binding: t; -*-

;;; macos title-bar fix
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))

;;; THEME

;; When not on mac, set theme to doom-nova (from doom-themes)
(unless (eq system-type 'darwin)
  (setq doom-theme 'doom-nova))

;; photometry
;; I want to be able to toggle "photometry" (automatic theme switching),
;; but I don't know how to properly build a module or code in elisp.

;; Here I start by defining an integer variable that tracks the state of
;; photometry being on or off.
(defvar photometry-mode 0)  ; photometry is recorded as "off"

;; Set light and dark theme choices here!
(defconst light-theme 'doom-solarized-light)
(defconst dark-theme 'doom-nova)

(defun photometry ()
  "Function for sensing light and changing themes based on apparent brightness
as reported through lmutracker executable."
  (let* ((current-light-sensor-reading
          (string-to-number
           (shell-command-to-string
            ;; this assumes you put the `lmutracker` executable
            ;; in your doom-config directory ("~/.doom.d/" or "~/.config/doom/")
            (concat doom-private-dir "lmutracker")))))
    (if (< current-light-sensor-reading 100000) ; test if environment is low-light
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
  (setq doom-theme dark-theme) ; starting (dark) theme
  (photometry/toggle))


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
