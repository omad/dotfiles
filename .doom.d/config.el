;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;

;;; Code:
(setq user-full-name "Damien Ayers"
      user-mail-address "damien@omad.net"
      org-directory "~/Dropbox/org/"
      doom-font (font-spec :family "Input Mono" :size 12)
      doom-big-font (font-spec :family "Fira Code" :size 20)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC)
(setq ns-use-thin-smoothing t)

;; Doom Settings
;; (load-theme 'doom-city-lights t)
(load-theme 'doom-one-light t)
;; (setq doom-theme 'doom-one-light)

;; Have treemacs follow the currently open file
(add-hook 'treemacs-mode #'treemacs-follow-mode)


(map! ;; Easier window movement
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      (:map vterm-mode-map
        ;; Easier window movement
        :i "C-h" #'evil-window-left
        :i "C-j" #'evil-window-down
        :i "C-k" #'evil-window-up
        :i "C-l" #'evil-window-right)

      (:map evil-treemacs-state-map
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right
        "M-j" #'multi-next-line
        "M-k" #'multi-previous-line))

(add-to-list 'tramp-methods
      '("yadm"
        (tramp-login-program "yadm")
        (tramp-login-args (("enter")))
        (tramp-remote-shell "/bin/sh")
        (tramp-remote-shell-args ("-c"))))

;; magit stuff
;; (setq +magit-hub-features t ;; I want the PR/issue stuff too!
;; +magit-hub-enable-by-default t) ;; And I want it on by default!


(setq deft-directory "~/Dropbox/org/"
      deft-extensions '("txt" "org" "md")
      deft-recursive t
      deft-use-filename-as-title nil
      deft-use-filter-string-for-filename t)


(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)


(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard. Thanks http://kitchingroup.cheme.cmu.edu/blog/2016/06/16/Copy-formatted-org-mode-text-from-Emacs-to-other-applications/"
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))


(setq magit-repository-directories
      '(("~/PycharmProjects/" . 1) ("~/dev/" . 1)))
(setq org-agenda-files '("~/Dropbox/org")
      org-refile-targets '((nil . (:maxlevel . 5))
                           (org-agenda-files . (:maxlevel . 5)))
      org-refile-allow-creating-parent-nodes 'confirm
      org-src-preserve-indentation t)
(setq
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-capture-templates (quote
                        (("t" "Todo" entry
                          (file "~/Dropbox/org/refile.org")
                          "* TODO %?"
                          ":LOGBOOK:"
                          "- Added: %U"
                          ":END:")
                         ("j" "Journal" entry
                          (file+olp+datetree "~/Dropbox/org/journal.org")
                          "* %?\n:LOGBOOK:\n- Entered on %U\n:END:\n%i")
                         ("m" "Meeting" entry
                          (file "~/Dropbox/org/refile.org")
                          "* MEETING: %? \n:MEETING:\n%U\n%a"
                          :clock-in t :clock-resume t)
                         ("w" "Work Todo" entry
                          (file "~/Dropbox/org/refile.org")
                          "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:\n%a\n%i"
                          :prepend t :clock-in t :clock-resume t))))


(defun dra-browse-url (url &optional new-window)
  "Call a url to open a url in the remote browser"
  (let ((url-request-method "POST")
        (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data (concat "url=" (url-hexify-string url))))

    (url-retrieve "http://localhost:9999/openurl" 'my-kill-url-buffer)))

(defun my-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(when (string-match-p "compute.internal$" (system-name))
  (setq browse-url-browser-function 'dra-browse-url))

;;; Not used

(defun my-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
          (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    args
                    "&")))
    ;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
    (url-retrieve url 'my-switch-to-url-buffer)))


(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

;; Override grayscale with sepiascale
(defun tao-theme-scale-to-colors (scale)
  "Create sepiascale from colors alist SCALE."
  (mapcar (lambda (it)
            (let* ((depth 10)
                   (saturation 1.03)
                   (r (+ it (* depth 1.8)))
                   (g (+ it (* depth 1.5)))
                   (b (* it saturation)))
              (format "#%02X%02X%02X"
                      (if (> r 255) 255 r)
                      (if (> g 255) 255 g))))
    (if (> b 255) 255 b) scale))

