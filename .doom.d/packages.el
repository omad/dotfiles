;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)


(package! org-journal)

;; https://gitlab.com/jabranham/mixed-pitch
(package! mixed-pitch)
(package! org-variable-pitch)
(package! tao-theme)
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))
