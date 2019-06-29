;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
<<<<<<< HEAD
;;

(setq-default
user-full-name    "Damien Ayers"
 user-mail-address "damien@omad.net"
 doom-font (font-spec :family "Input Mono" :size 12)
 doom-big-font (font-spec :family "Fira Code" :size 20))

;; Doom Settings
(load-theme 'doom-city-lights t)

;; Have treemacs follow the currently open file
(add-hook 'treemacs-mode #'treemacs-follow-mode)



;; magit stuff
(setq +magit-hub-features t ;; I want the PR/issue stuff too!
+magit-hub-enable-by-default t) ;; And I want it on by default!


(setq deft-directory "~/Dropbox/org/"
      deft-extensions '("txt" "org" "md")
      deft-recursive t
      deft-use-filename-as-title nil
      deft-use-filter-string-for-filename t
      )
