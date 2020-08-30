;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;;; Code:
(setq
 user-full-name "Damien Ayers"
 user-mail-address "damien@omad.net"

 doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
 doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
 doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
 doom-scratch-initial-major-mode 'lisp-interaction-mode
 org-directory "~/org/"
 org-agenda-files nil
 ;; org-super-agenda-groups '((:name "Today"
 ;;                             :time-grid t
 ;;                             :scheduled today)
 ;;                           (:name "Due today"
 ;;                                   :deadline today)
 ;;                           (:name "Important"
 ;;                                   :priority "A")
 ;;                           (:name "Overdue"
 ;;                                   :deadline past)
 ;;                           (:name "Due soon"
 ;;                                   :deadline future)
 ;;                           (:name "Big Outcomes"
 ;;                            :tag "bo"))
 projectile-project-search-path '("~/dev/")
 auto-save-visited-mode t
 auto-save-default t ; Turn on Automatic Saves
 calendar-date-style 'european ; American date format is the worst
 org-log-done 'time
 org-log-into-drawer t)
 ;; doom-theme 'doom-one-light             ;
 ;; ivy-read-action-function #'ivy-hydra-read-action)

(setq doom-theme 'doom-vibrant)

;; (setq doom-theme 'zaiste)

;; Under xpra, M-SPC doesn't get sent :(
;; But seemingly neither does Win-SPC
;;(keyboard-translate ?\M-SPC C-g)

(after! org
  (setq
   org-bullets-bullet-list '("⁖")
   org-ellipsis " ... "
   org-todo-keyword-faces
   '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
     ("WAITING" :foreground "#9f7efe" :weight normal :underline t)
     ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
     ("DONE" :foreground "#50a14f" :weight normal :underline t)
     ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
   org-priority-faces '((65 :foreground "#e45649")
                        (66 :foreground "#da8548")
                        (67 :foreground "#0098dd"))))

;; (setq display-line-numbers-type nil)
;; or
;; (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook))
(remove-hook! '(text-mode-hook)
              #'display-line-numbers-mode)

;; moved from custom set variables
(use-package! org-journal
      :custom
      (org-journal-enable-agenda-integration t)
      (org-journal-file-type 'daily)
      (org-journal-dir "~/org/journal/")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-time-prefix "* ")
      (org-journal-date-format "%A, %d %B %Y"))

(after! python
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

(setq! auth-sources '("secrets:Login" "~/.authinfo.gpg" "~/.authinfo"))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))

(when IS-MAC
  (setq ns-use-thin-smoothing t))



;; Use MS Python Language Server by default
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))

(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

;; Have treemacs follow the currently open file
(add-hook 'treemacs-mode #'treemacs-follow-mode)

;; With some creative use of X401 and xrandr, this finally works in Windows
(add-hook 'window-setup-hook #'toggle-frame-maximized)

;; (map! ;; Easier window movement
;;       :n "C-h" #'evil-window-left
;;       :n "C-j" #'evil-window-down
;;       :n "C-k" #'evil-window-up
;;       :n "C-l" #'evil-window-right

;;       (:map vterm-mode-map
;;         ;; Easier window movement
;;         :i "C-h" #'evil-window-left
;;         :i "C-j" #'evil-window-down
;;         :i "C-k" #'evil-window-up
;;         :i "C-l" #'evil-window-right)

;;       (:map evil-treemacs-state-map
;;         "C-h" #'evil-window-left
;;         "C-l" #'evil-window-right
;;         "M-j" #'multi-next-line
;;         "M-k" #'multi-previous-line))

;; With this config, use (magit-status "/yadm::"). If you find issue with Emacs 27 ;
;; and zsh, trying running (setenv "SHELL" "/bin/bash").
(after! tramp
  (add-to-list 'tramp-methods
    '("yadm"
      (tramp-login-program "yadm")
      (tramp-login-args (("enter")))
      (tramp-login-env (("SHELL") ("/bin/sh")))
      (tramp-remote-shell "/bin/sh")
      (tramp-remote-shell-args ("-c")))))
(map! :leader "g." (cmd! (magit-status "/yadm::")))

(map! :leader "nj." (cmd! (org-journal-open-current-journal-file)))


(set-popup-rule! "^\\*eww\\*" :ignore t)

(add-hook! 'markdown-mode-hook
           'auto-fill-mode
           'flyspell-mode)


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

(setq!
 magit-repository-directories '(("~/PycharmProjects/" . 1) ("~/dev/" . 1))
 org-roam-directory "~/org"
 org-roam-buffer-width 0.2
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-src-preserve-indentation t
 org-blank-before-new-entry '((heading) (plain-list-item))
 org-capture-templates '(("t" "Todo" entry
                          (file "~/org/refile.org")
                          "* TODO %?"
                          ":LOGBOOK:"
                          "- Added: %U"
                          ":END:")
                         ("j" "Journal" entry
                          (file+olp+datetree "~/org/journal.org")
                          "* %?\n:LOGBOOK:\n- Entered on %U\n:END:\n%i")
                         ("m" "Meeting" entry
                          (file "~/org/refile.org")
                          "* MEETING: %? \n:MEETING:\n%U\n%a"
                          :clock-in t :clock-resume t)
                         ("w" "Work Todo" entry
                          (file "~/org/refile.org")
                          "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:\n%a\n%i"
                          :prepend t :clock-in t :clock-resume t))

 org-refile-use-outline-path 'file
 org-refile-targets '()
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-targets '(
                      (nil . (:maxlevel . 5))
                      (org-agenda-files . (:maxlevel . 5))
                      ("next.org" :level . 0)
                      ("someday.org" :level . 0)
                      ("reading.org" :level . 1)
                      ("projects.org" :maxlevel . 1)))

(after! org
  (add-hook! 'org-mode-hook
             'auto-fill-mode
             'eldoc-mode
             'hide-mode-line-mode
             'flyspell-mode))


(defun +org-eldoc-get-breadcrumb-no-properties (string)
  "Remove properties from STRING."
  (when string
    (substring-no-properties string)))

(advice-add 'org-eldoc-get-breadcrumb :filter-return #'+org-eldoc-get-breadcrumb-no-properties)


(defun dra/subtree-to-new-file ()
  (interactive)
  "sloppily assists in moving an org subtree to a new file"
  (org-copy-subtree nil t)
  ;;; This long setq statement gets the title of the first heading, to use as a default filename for the new .org file.
  (setq first-heading
        (with-temp-buffer
          (yank)
          (goto-char (point-min))
          (search-forward " " nil nil 1)
          (setq title-start (point))
          (end-of-visual-line)
          (setq title-end (point))
          (setq first-heading (buffer-substring title-start title-end))))
  (setq def-filename (concat first-heading ".org"))
  (let ((insert-default-directory t))
    (find-file-other-window
     (read-file-name "Move subtree to file:" def-filename)))
  (org-paste-subtree)
  ;;; this final command adds the new .org file to the agenda
  (org-agenda-file-to-front))

(defun dra/move-org-journal-to-file ()
  "Move an org-journal date heading to a new file."
  (interactive)
  (org-copy-subtree nil t)
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    (re-search-forward "[[:digit:]/]+$" nil)
    (setq backwards-date (match-string 0)))
  (let
      ((def-filename (concat (s-join "" (nreverse (split-string backwards-date "/"))) ".org"))
       (insert-default-directory t))
      (find-file-other-window
        (read-file-name "Move subtree to file:" def-filename)))
  (org-paste-subtree))


(use-package deft
  :after org
  :custom
  (deft-directory "~/org/")
  (deft-extensions '("txt" "org" "md"))
  (deft-recursive t)
  (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\)$\\|\\..*")
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t))




(provide 'config)
;;; config.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (lsp-python-ms))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
