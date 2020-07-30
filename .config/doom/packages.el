;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)


(package! org-sync)
(package! ob-http)

(package! olivetti)
;; (package! tao-theme)
;;
;; https://gitlab.com/jabranham/mixed-pitch
;; (package! mixed-pitch)

;; (package! org-super-agenda)
;; (package! org-variable-pitch)
;; (package! org-pretty-table)
;; (package! org-bullets)
(package! org-cliplink)
;;   :hook
;;   ;; If you want it in all text modes:
;;   (text-mode . mixed-pitch-mode))
;;
;;
; This makes manual pages nicer to look at :) Variable pitch fontification + colouring
(package! info-colors :pin "47ee73cc19...")
