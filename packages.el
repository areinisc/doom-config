;;; packages.el --- private package config  -*- lexical-binding: t; -*-

;;;
;;; Package! calls
;;;

(package! fireplace)

(package! atomic-chrome)

(package! org-web-tools)

;;; Trying Henrik's macos patch.
;(package! exec-path-from-shell :disable t)
;;; have to edit /usr/local/Cellar/emacs-plus/Emacs.app/Contents/MacOS/Emacs
;;; change shebang line from #!/usr/bin/env bash to #!/usr/local/bin/bash
;;; Actually we're changing to #!/bin/bash and also adding -l to exec like so:
;;; exec "$SHELL" -l -c "$pwd/RunEmacs $args"


;;;
;;; Useless example code cruft
;;;

;; And paredit somehow already exists.
;; Let's get paredit up in here.
;; (package! paredit)

;; Apparently we already have slime too.
;;; quicklisp + SLIME + SBCL = LISP development
;; Add SLIME from github repo
;; (package! slime
;;   :recipe (:fetcher github
;;            :repo "slime/slime"
;;            :files ("*")))

;; No need to install doom-themes, they come with doom!
;; Attempt to install doom-themes package
;; (package! doom-themes
;;   :recipe (:fetcher github
;;            :repo "hlissner/emacs-doom-themes"
;;            :files ("*")))
