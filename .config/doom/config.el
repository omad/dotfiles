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
 org-super-agenda-groups '((:name "Today"
                             :time-grid t
                             :scheduled today)
                           (:name "Due today"
                                   :deadline today)
                           (:name "Important"
                                   :priority "A")
                           (:name "Overdue"
                                   :deadline past)
                           (:name "Due soon"
                                   :deadline future)
                           (:name "Big Outcomes"
                            :tag "bo"))
 projectile-project-search-path '("~/dev/"))

;; moved from custom set variables
(setq
 auto-save-visited-mode t
 calendar-date-style (quote european)
 org-journal-date-format "%A, %d/%m/%Y"
 org-journal-dir "~/Dropbox/org/journal/"
 org-journal-enable-agenda-integration t
 org-journal-file-format "%Y%m%d.org"
 org-journal-file-type (quote daily)
 org-log-done (quote time)
 org-log-into-drawer t)

(after! python
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")))

(setq! auth-sources '("secrets:Login" "~/.authinfo.gpg" "~/.authinfo"))

(when IS-LINUX
  (font-put doom-font :weight 'semi-light))
(when IS-MAC
  (setq ns-use-thin-smoothing t))

;; Doom Settings
(load-theme 'doom-city-lights t)
;; (load-theme 'doom-one-light t)

;; (setq doom-theme 'doom-one-light)

;; Have treemacs follow the currently open file
(add-hook 'treemacs-mode #'treemacs-follow-mode)

;; With some creative use of X401 and xrandr, this finally works in Windows
(add-hook 'window-setup-hook #'toggle-frame-maximized)

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

(set-popup-rule! "^\\*eww\\*" :ignore t)



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


(setq!
 org-roam-directory "~/org"
 magit-repository-directories '(("~/PycharmProjects/" . 1) ("~/dev/" . 1))


 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-files '("~/org")
 org-refile-targets '((nil . (:maxlevel . 5))
                      (org-agenda-files . (:maxlevel . 5)))
 org-refile-allow-creating-parent-nodes 'confirm
 org-src-preserve-indentation t
 org-blank-before-new-entry (quote ((heading) (plain-list-item)))
 org-capture-templates (quote
                        (("t" "Todo" entry
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
                          :prepend t :clock-in t :clock-resume t))))



;; Task lists

(defun +org-prettify-task-symbols-setup ()
  "Prettify task list symbols."
  (dolist (symbol '(("TODO"     . ?⚑)
                    ("DOING"    . ?⚐)
                    ("CANCELED" . ?✘)
                    ("DONE"     . ?✔)))
    (cl-pushnew symbol prettify-symbols-alist :test 'equal)))

(load! "org-pretty-table")

(defun +org-eldoc-get-breadcrumb-no-properties (string)
  "Remove properties from STRING."
  (when string
    (substring-no-properties string)))
(advice-add 'org-eldoc-get-breadcrumb :filter-return #'+org-eldoc-get-breadcrumb-no-properties)

(after! org
  (require 'org-sync)
  (require 'org-sync-github)
  (add-hook! 'org-mode-hook 'auto-fill-mode
                             'eldoc-mode
                             'hide-mode-line-mode
                             'flyspell-mode
                             'org-variable-pitch-minor-mode
                             'org-pretty-table-mode
                             '+org-prettify-task-symbols-setup
                             (setq display-line-numbers nil)))




(defun subtree-to-new-file ()
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


;; (after! org-roam
;;   (map! :leader
;;         :prefix "n"
;;         :desc "org-roam" "l" #'org-roam
;;         :desc "org-roam-insert" "i" #'org-roam-insert
;;         :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
;;         :desc "org-roam-find-file" "f" #'org-roam-find-file
;;         :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;         :desc "org-roam-insert" "i" #'org-roam-insert
;;         :desc "org-roam-capture" "c" #'org-roam-capture))

(use-package deft
  :after org
  :custom
  (deft-directory "~/org/")
  (deft-extensions '("txt" "org" "md"))
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t))


(use-package org-journal
      :custom
      (org-journal-dir "~/org/journal/")
      (org-journal-date-prefix "#+TITLE: ")
      (org-journal-file-format "%Y-%m-%d.org")
      (org-journal-date-format "%A, %d %B %Y"))


(provide 'config)
;;; config.el ends here
