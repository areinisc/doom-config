;;; private-config --- My personal DOOM config file.  -*- lexical-binding: t; -*-

;;;;
;;;; General Settings and Doom Things
;;;;

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Set user name, big-font, etc.
(setq user-full-name    "Alex Reinisch"
      user-mail-address "alex.p.reinisch@gmail.com"

      ;; IHO, modern editors have trained a bad habit into us all: a burning
      ;; need for completion all the time -- as we type, as we breathe, as we
      ;; pray to the ancient ones -- but how often do you *really* need that
      ;; information? I say rarely. So opt for manual completion:
      company-idle-delay nil)

      ;; +pretty-code-enabled-modes '(emacs-lisp-mode org-mode)
      ;; +format-on-save-enabled-modes '(not emacs-lisp-mode)

;; change easy-motion jump targets for Colemak layout
;; only do this if I'm the system user
(when (or (string= user-full-name  "Alex Reinisch")
          (string= user-login-name "alexreinisch")
          (string= user-login-name "areinisch"))
  (setq avy-keys '(?a ?r ?s ?t ?h ?n ?e ?i ?o)))

;; atomic-chrome
(use-package! atomic-chrome
  :after-call focus-out-hook
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))

;; ;;;###autoload
;; (defun +hlissner/find-notes-for-project (&optional arg)
;;   "TODO"
;;   (interactive "P")
;;   (let ((project-root (doom-project-name 'nocache))
;;         (default-directory (expand-file-name "projects/" org-directory)))
;;     (if arg
;;         (call-interactively #'find-file)
;;       (find-file
;;        (expand-file-name (concat project-root ".org"))))))

;; TODO: Actually make these functions be autoloaded.
(defun +areinisch/insert-inactive-time-stamp ()
  "Convenience function for inserting inactive time stamp at point."
  (interactive)
  (org-time-stamp-inactive '(16)))

;; Save and exit for journal entry by @dhruvparamhans
(defun +areinisch/org-journal-save-entry-and-exit ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))
;; (define-key org-journal-mode-map (kbd "C-x C-s") 'org-journal-save-entry-and-exit)

;; Toggle #'fireplace
(defun +areinisch/fireplace-toggle ()
  "Convenience function for toggling fireplace on/off."
  (interactive)
  (if (get '+areinisch/fireplace-toggle 'state)
      (progn
        (fireplace)
        (put '+areinisch/fireplace-toggle 'state nil))
    (progn
      (fireplace-off)
      (put '+areinisch/fireplace-toggle 'state t))))

;; Paste from default/system register
(defun +areinisch/system-paste ()
  "Paste from the system/default register. Basically just shorthand for =C-r +=."
  (interactive)
  (evil-paste-from-register ?+))

;;;;
;;;; Font
;;;;

;; In case we use this config on a system without these fonts, fail silently
(setq doom-font (face-attribute 'default :font)
      ;; doom-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light)
      ;; doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "sans" :size 13))
      ;; doom-variable-pitch-font (font-spec :family "iA Writer Quattro V" :size 14))
      ;; doom-variable-pitch-font (font-spec :family "Fira Sans")
      ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      ;; doom-font (face-attribute 'default :font))

;; TODO: Find a way to ensure `doom-big-font` choice exists
(if (eq system-type 'darwin)
    (setq doom-big-font (font-spec :family "Source Code Pro" :size 19))
    (setq doom-big-font (font-spec :family "Noto Mono"       :size 19)))

(custom-set-faces!
  `(markdown-code-face :background ,(doom-darken 'bg 0.075)))


;;;;
;;;; THEME
;;;;

;; Set light and dark theme choices here!
(defvar light-theme 'doom-solarized-light) ; doom-solarized-light doom-nord-light doom-one-light
(defvar dark-theme  'doom-dracula)         ; doom-dracula doom-peacock doom-one doom-nord
(defvar mac-default-theme dark-theme
  "Controls whether default theme is `dark-theme` or `light-theme`.")
;; System agnostic default theme choice goes here:
(setq doom-theme dark-theme)           ; set default theme choice

;; Function for hot-swapping between light and dark themes.
(defun +areinisch/toggle-theme ()
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
        (t (message "Can't toggle theme: not using light-theme (%s) or dark-theme (%s)." light-theme dark-theme))))


;;;;
;;;; macOS specific
;;;;

(when (eq system-type 'darwin)          ; should be the same as (when IS-MAC ...)
  (setq ns-use-thin-smoothing t         ; Use thinner strokes for font smoothing.
        ns-right-option-modifier nil)   ; unbind right option key to allow umlauting
  ;; macOS title-bar fix
  ;; TODO Figure out how to redraw the title-bar to change ns-appearance with theme.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ; CDR: light | dark

  ;; Set this to nil if you don't want automagical theme switching.
  (defconst use-photometry t
    "Set this to `t` to use photometry, set it to `nil` to not use photometry.")
  (setq doom-theme mac-default-theme)   ; set theme to mac default (dark)

  ;; Photometry
  (when use-photometry
    ;; I want to be able to toggle "photometry" (automatic theme switching),
    ;; but I don't know how to properly build a module or code in elisp.
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
      (if (not photometry-state)                                                 ; test if photometry is currently off
          (progn (message "Photometry ON.")                                      ; print message about new state (ON)
                 (setq photometry-state (not photometry-state)                   ; update state variable
                       photometry-timer (run-with-idle-timer 3 t #'photometry))) ; start timer to run photometry during each idle-time > 3 seconds
        (progn (message "Photometry OFF")                                        ; print message about new state (OFF)
               (setq photometry-state (not photometry-state))                    ; update state variable
               (cancel-timer photometry-timer))))                                ; cancel timer object controlling photometry calls

    ;; Hooks to run photometry at startup.
    (add-hook! 'window-setup-hook #'photometry)        ; run photometry once after doom init
    (add-hook! 'window-setup-hook #'photometry/toggle) ; toggle photometry on

    ;; Keybinding for toggling photometry with `SPC t p`
    (map!
     (:leader
       (:prefix "t"
         :desc "Photometry"   :n "p" #'photometry/toggle)))))


;;;;
;;;; KEYBINDS
;;;;

(map!
 (:leader
   :desc "Paste from reg[+]"   :n "P" #'+areinisch/system-paste
   (:prefix "n"
     (:prefix "j"
       :desc "New todo entry"  :n "t" #'org-journal-new-scheduled-entry))
 ;; (:prefix "o"
 ;;   :desc "undo-tree-visualize"   :n "u" #'undo-tree-visualize)
 ;; (:prefix "p"
 ;;   :desc "Browse project notes"  :n "n" #'+hlissner/find-notes-for-project)
   (:prefix "t"
     :desc "Auto hard wrapping"      :n "a" #'auto-fill-mode
     :desc "Theme (dark/light)"      :n "t" #'+areinisch/toggle-theme
     ;; :desc "toggle visual-line-mode" :n "v" #'visual-line-mode
     :desc "Light/extinguish fire"   :n "Z" #'+areinisch/fireplace-toggle))
 (:after fireplace
   :localleader
   (:map fireplace-mode-map
     :desc "Move fire further up"   "u" #'fireplace-up
     :desc "Push fire further down" "d" #'fireplace-down
     :desc "Toggle smoke on/off"    "s" #'fireplace-toggle-smoke
     :desc "Toggle sound on/off"    "S" #'fireplace-toggle-sound
     :desc "Put out the fire"       "q" #'+areinisch/fireplace-toggle))
 (:after org
   :localleader
   (:map org-journal-mode-map
     (:prefix ("z" . "+cuztom")
       :desc "Save and exit"              "q" #'+areinisch/org-journal-save-entry-and-exit))
   (:map org-mode-map
     (:prefix ("z" . "+cuztom")
       :desc "Insert inactive time-stamp" "i" #'+areinisch/insert-inactive-time-stamp
       :desc "Copy subtree to clipboard"  "y" #'org-copy-subtree))))
 ;; (:after smartparens
 ;;   (:map smartparens-mode-map
 ;;     ",s" #'sp-splice-sexp
 ;;     ",f" #'sp-forward-slurp-sexp
 ;;     ",w" #'sp-wrap-round))
 ;; (:after sly
 ;;   :localleader
 ;;   :map sly-mode-map
 ;;   (:prefix ("g" . "go"))
 ;;   (:prefix ("h" . "help")
 ;;     :desc "sly-apropos-all" "A" #'sly-apropos-all)
 ;;   (:prefix ("c" . "compile"))
 ;;   (:prefix ("m" . "macrostep"))
 ;;   (:prefix ("t" . "trace")
 ;;     :desc "fetch traces"         "G"   #'sly-trace-dialog-fetch-traces
 ;;     :desc "clear fetched traces" "C-k" #'sly-trace-dialog-clear-fetched-traces
 ;;     :desc "fetch traces"         "g"   #'sly-trace-dialog-fetch-status
 ;;     :desc "fetch traces"         "q"   #'quit-window)))


;;;;
;;;; Modules
;;;;

;;; completion/ivy
;; ;; Henrik prefers search matching to be ordered; it's more precise
;; (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;;; feature/evil
;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(when (featurep! :feature evil)
  (after! evil
    ;; Make '<' and '>' indent/de-dent only two spaces
    (setq evil-shift-width 2)))

;;; lang/common-lisp
;; (after! sly
;;   (setq sly-complete-symbol-function 'sly-flex-completions))

;;; lang/org
;; Set org agenda file locations.
(setq org-directory      (expand-file-name "~/org/")
      online-courses-dir (expand-file-name "~/Documents/online-courses/")
      deac-dir           (concat online-courses-dir "dog-emotion-and-cognition/")
      dog-dir            (concat online-courses-dir "DOGx003/")
      mindfulness-dir    (concat online-courses-dir "mindfulness/")
      pfa-dir            (concat online-courses-dir "psychological-first-aid/")
      org-archive-location  (concat org-directory ".archive/%s::")
      org-journal-enable-agenda-integration t)
      ;; org-roam-directory   (concat org-directory "roam/")

;; Change default org display settings
(setq org-ellipsis " .▾▼▾."
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷"))
  ;; org-bullets-bullet-list '("#")

;; TODO these todo state tags don't work the same way anymore
;; see [[file:~/.emacs.d/modules/lang/org/config.el::(setq org-todo-keywords]]
;; Add todo-state-change triggers
;; (setq org-todo-state-tags-triggers
;;       (quote (("CANCELLED" ("CANCELLED" . t))
;;               ("WAITING" ("WAITING" . t))
;;               ("LATER" ("WAITING") ("LATER" . t))
;;               (done ("WAITING") ("LATER"))
;;               ("TODO" ("WAITING") ("CANCELLED") ("LATER"))
;;               ("NEXT" ("WAITING") ("CANCELLED") ("LATER"))
;;               ("DONE" ("WAITING") ("CANCELLED") ("LATER")))))

(after! org                           ; don't run until org is loaded
  (setq org-agenda-files (list org-directory
                               deac-dir
                               dog-dir
                               mindfulness-dir
                               pfa-dir))

  ;; Start temporary org buffers in insert state rather than normal state
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ;; ;; Make agenda popup bigger
  ;; (set-popup-rule! "^\\*Org Agenda" :side 'left :size 0.5 :select t :ttl nil)

  ;; Add habits module
  (add-to-list 'org-modules 'org-habit t)
  ;; Change default org-habits settings
  (after! org-habit
    (setq org-habit-graph-column   54   ; The absolute column at which to insert habit consistency graphs. N.B. consistency graphs will overwrite anything else in the buffer.
          org-habit-preceding-days 19   ; Number of days before today to appear in consistency graphs.
          org-habit-following-days 7    ; Number of days after today to appear in consistency graphs.
          org-habit-show-habits-only-for-today t ; If non-nil, only show habits on today's agenda, and not for future days. N.B. even when shown for future days, the graph is always relative to the current effective date.
          org-habit-show-done-always-green nil ; Non-nil means DONE days will always be green in the consistency graph. It will be green even if it was done after the deadline.
          org-habit-show-all-today nil  ; If non-nil, will show the consistency graph of all habits on today's agenda, even if they are not scheduled.
          org-habit-show-habits    t))) ; If non-nil, show habits in agenda buffers.
