;;;  -*- lexical-binding: t; -*-

(advice-add #'recentf-cleanup :around #'doom*shut-up)

;;;;
;;;; FIXUP for macos
;;;;

;;; macos title-bar fix
(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))


;;;;
;;;; THEME
;;;;

;; Set light and dark theme choices here!
(defconst light-theme 'doom-solarized-light) ; doom-nord-light doom-one-light
(defconst dark-theme  'doom-dracula)         ; doom-dracula doom-peacock doom-one doom-nord
(defconst use-photometry t
  "Set this to `t` to use photometry, set it to `nil` to not use photometry.")
(defconst mac-default-theme dark-theme
  "Controls whether default theme is dark or light.")

(defun toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond ((eq doom-theme dark-theme)
         (message "Toggling to light-theme: %s" light-theme)
         (setq doom-theme light-theme)
         (doom/reload-theme))
        ((eq doom-theme light-theme)
         (message "Toggling to dark-theme: %s" dark-theme)
         (setq doom-theme dark-theme)
         (doom/reload-theme))
        (t (message "Toggling theme is not possible. Theme is not currently light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))

;;; photometry
;; I want to be able to toggle "photometry" (automatic theme switching),
;; but I don't know how to properly build a module or code in elisp.

;; Define integer variable to track photometry state (on/off; true/false)
(defvar photometry-state nil
  "Tracks whether photometry module is on (true) or off (false).")
(defvar photometry-timer nil "Timer object used when photometry is on.")

(defun photometry ()
  "Sense light and change themes.
Function for sensing light and changing themes based on apparent brightness
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
    (if (< current-light-sensor-reading 425000) ; test if environment is low-light
        (unless (eq doom-theme dark-theme) ; if theme is not yet dark
          (setq doom-theme dark-theme)     ; change to dark theme
          (doom/reload-theme))
      (when (eq doom-theme dark-theme)     ; if theme is dark
        (setq doom-theme light-theme)      ; change to light theme
        (doom/reload-theme)))))

(defun photometry/toggle ()
  "Toggle photometry on/off.
Photometry is used to change the theme based on ambient light sensor readings."
  (interactive)
  (if (not photometry-state)                                                  ; test if photometry is currently off
      (progn (message "Photometry ON.")                                       ; print message about new state (ON)
             (setq photometry-state (not photometry-state)                    ; update state variable
                   photometry-timer (run-with-idle-timer 3 t #'photometry)))  ; start timer to run photometry during each idle-time > 3 seconds
    (progn (message "Photometry OFF")                                         ; print message about new state (OFF)
           (setq photometry-state (not photometry-state))                     ; update state variable
           (cancel-timer photometry-timer))))                                 ; cancel timer object controlling photometry calls

;; Add keybind so photometry can be toggled with `SPC t p`
(map! (:leader
        (:desc "toggle" :prefix "t"
          :desc "Photometry"   :n "p" #'photometry/toggle
          :desc "toggle-theme" :n "t" #'toggle-theme)))

;; When on mac, use photometry for automatic theme adjustment
(when (eq system-type 'darwin)
  (setq doom-theme mac-default-theme)                       ; set theme to mac default (dark)
  (when use-photometry
    (add-hook! 'window-setup-hook #'photometry)           ; run photometry once after doom init
    (add-hook! 'window-setup-hook #'photometry/toggle)))  ; toggle photometry on

;; When not on mac, set theme to doom-nova (from doom-themes)
(unless (eq system-type 'darwin)
  (setq doom-theme 'doom-nova))


;;;;
;;;; PLUGINS
;;;;

;;; ein
;; (after! ein
;;   (set! :ein-notebook-dir "~/Documents"))

;;; magit
;; Get magit buffer in a split rather than its own window---like it used to.
;; (after! magit
;;   (setq magit-display-buffer-function #'magit-display-buffer-traditional))
;; try 3:
(defun +magit|update-vc-post-refresh ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (let ((revert-buffer-in-progress-p t))
          (vc-refresh-state)))))
(add-hook 'magit-post-refresh-hook #'+magit|update-vc-post-refresh)
;; try 2:
;; (add-hook! 'magit-pre-refresh-hook  (setq auto-revert-check-vc-info t))
;; (add-hook! 'magit-post-refresh-hook (setq auto-revert-check-vc-info nil))
;; try 1:
;; (defun +magit|update-vc-post-refresh ()
;;   (cl-loop with project-root = (magit-toplevel)
;;            for buf in (buffer-list)
;;            for path = (buffer-file-name buf)
;;            if (and path (file-in-directory-p path project-root))
;;            do (with-current-buffer buf
;;                 (when (and (featurep 'git-gutter) git-gutter-mode)
;;                   (git-gutter))
;;                 (when-let* ((backend (vc-backend path)))
;;                   (vc-file-setprop
;;                    path 'vc-state
;;                    (vc-call-backend backend 'state path))))))
;; (add-hook 'magit-post-refresh-hook #'+magit|update-vc-post-refresh)
;; try 0.5 (old-modeline):
;; (add-hook 'after-revert-hook #'+doom-modeline--update-vcs)

;;; paredit
;; (def-package! paredit
;;   :commands (paredit-mode enable-paredit-mode)
;;   :hook (lisp-mode . enable-paredit-mode))
;; Option B)
;; Doesn't enforce paredit mode for lisp files, just autoloads it.
;; (def-package! paredit
;;   :defer t
;;   :init (load "paredit-autoloads" nil t))

;;; parinfer
;; parinfer-double-quote doesn't appear to be doing anything particularly
;; magical that smartparens doesn't already do.
;; (after! parinfer
;;   (define-key parinfer-mode-map "\"" nil))

;;; slime
;; quicklisp + SLIME + SBCL = LISP development
;; define slime package as proscribed by hlissner:
;; Option A)
;; (def-package! slime
;;   :defer t
;;   :init (load "slime-autoloads" nil t)
;;   :config
;;   ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;         slime-contribs '(slime-fancy)))
;; Option B)
;; (def-package! slime
;;   :commands (slime slime-mode slime-connect slime-selector slime-setup)
;;   :hook (lisp-mode . slime-lisp-mode-hook)
;;   :config
;;   (load (expand-file-name "~/quicklisp/slime-helper.el"))
;;   (setq inferior-lisp-program "/usr/local/bin/sbcl"
;;         slime-contribs '(slime-fancy)))

;;; sly
;; (after! sly
;;   (set-popup-rule!      "^\\*sly" :quit nil :ttl nil)
;;   (set-repl-handler!    'lisp-mode #'sly-mrepl)
;;   (set-lookup-handlers! 'lisp-mode :documentation #'sly-documentation))
;; (package--compile (cadr (assq 'sly package-alist))) ; errors, but also suggested for mismatch fix
;; (byte-compile-file (locate-library "sly.el"))  ; eval this when versions mismatch

;;;;
;;;; KEYBINDS
;;;;

;;; helm
;; restores behavior of backspace going up a directory at a time
(map! :after helm-files
      :map helm-find-files-map
      "<DEL>" #'helm-find-files-up-one-level)

;;; smartparens
;; (map!
;;   (:after smartparens
;;     (:map smartparens-mode-map
;;       ",s" #'sp-splice-sexp
;;       ",f" #'sp-forward-slurp-sexp
;;       ",w" #'sp-wrap-round)))
;; (map!
;;  (:map lisp-mode-shared-map
;;    (:prefix "C-,"
;;      "s"  #'sp-splice-sexp
;;      "w"  #'sp-wrap-round
;;      "f"  #'sp-forward-slurp-sexp
;;      "b"  #'sp-forward-barf-sexp)))
