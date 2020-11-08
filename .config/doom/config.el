;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;;; Code:
(setq!
 user-full-name "Damien Ayers"
 user-mail-address "damien@omad.net"

 doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
 doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
 doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
 doom-scratch-initial-major-mode 'lisp-interaction-mode
 projectile-project-search-path '("~/dev/")
 auto-save-visited-mode t
 auto-save-default t ; Turn on Automatic Saves
 calendar-date-style 'european ; American date format is the worst
 ispell-dictionary "en_AU"
 )


(setq doom-theme 'doom-vibrant)

;; Under xpra, M-SPC doesn't get sent :(
;; But seemingly neither does Win-SPC
;;(keyboard-translate ?\M-SPC C-g)


(remove-hook! text-mode
  #'display-line-numbers-mode)

;; moved from custom set variables
(after! org-journal
  (setq!
   org-journal-enable-agenda-integration t
   org-journal-file-type 'daily
   org-journal-date-prefix "#+TITLE: "
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-time-prefix "* "
   org-journal-date-format "%A, %d %B %Y"))

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

;; Have treemacs follow the currently open file
(add-hook! 'treemacs-mode #'treemacs-follow-mode)

;; With some creative use of X401 and xrandr, this finally works in Windows
(add-hook! 'window-setup-hook #'toggle-frame-maximized)

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

(map! :leader :desc "yadm status" "g." (cmd! (magit-status "/yadm::")))

(map! :leader "nj." (cmd! (org-journal-open-current-journal-file)))


(set-popup-rule! "^\\*eww\\*" :ignore t)

(add-hook! markdown-mode
           'auto-fill-mode
           'flyspell-mode)


(defun dra/formatted-copy ()
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

(after! magit
  (setq!
   magit-repository-directories '(("~/PycharmProjects/" . 1) ("~/dev/" . 1))))

(setq org-roam-directory "~/org")

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (org-narrow-to-subtree)
  (goto-char (point-max)))

(setq org-capture-templates '())
(after! org
  (setq!
   org-log-done 'time
   org-log-into-drawer t
   org-directory "~/org/"
   org-agenda-files nil
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
                        (67 :foreground "#0098dd"))
   org-roam-buffer-width 0.2
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-src-preserve-indentation t
   org-blank-before-new-entry '((heading) (plain-list-item))
   org-capture-templates '(
                           ;; ("t" "Todo" entry
                           ;;  (file "~/org/refile.org")
                           ;;  "* TODO %?"
                           ;;  ":LOGBOOK:"
                           ;;  "- Added: %U"
                           ;;  ":END:")
                           ("t" "Todo" entry (function org-journal-find-location)
                            "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:"
                            :empty-lines-before 1)
                           ("j" "Journal entry" plain (function org-journal-find-location)
                            "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                            :jump-to-captured t :immediate-finish t)
                           ;; ("j" "Journal" entry
                           ;;  (file+olp+datetree "~/org/journal.org")
                           ;;  "* %?\n:LOGBOOK:\n- Entered on %U\n:END:\n%i")
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
                        ("projects.org" :maxlevel . 1))
   company-backends '(company-capf)))


(add-hook! org-mode
           'auto-fill-mode
           'eldoc-mode
           'hide-mode-line-mode
           'flyspell-mode)


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


(use-package! deft
  :after org
  :custom
  (deft-directory "~/org/")
  (deft-extensions '("txt" "org" "md"))
  (deft-recursive t)
  (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\)$\\|\\..*")
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t))

;; I don't know why, but backtick (`) isn't included for usage with evil-surround or evil-embrace
;; I would use it a lot, so add it in and hope it doesn't break anything.
;; Blergh, it doesn't match properly, wtf
;;(add-to-list 'evil-embrace-evil-surround-keys ?\`)

(fset 'dra/convert-markdown-to-org-link
      (kmacro-lambda-form [?f ?\[ ?d ?f ?\] ?% ?p ?% ?h ?c ?\C-g ?\C-g ?l ?c ?s ?\) ?\] ?v ?2 ?f ?\] ?S ?\]] 0 "%d"))

;; While we’re modifying the modeline, LF UTF-8 is the default file encoding,
;; and thus not worth noting in the modeline. So, let’s conditionally hide it.

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Info Colours
;;
(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook! 'Info-selection-hook 'info-colors-fontify-node)

(add-hook! 'Info-mode-hook #'mixed-pitch-mode)

(add-hook! (gfm-mode markdown-mode) #'mixed-pitch-mode)
;; Turn off hard line wraps in markdown and GFM
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)

;; Org Chef(use-package! org-chef
;;

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

;; While in an ivy mini-buffer C-o shows a list of all possible actions one may
;; take. By default this is #'ivy-read-action-by-key however a better interface
;; to this is using Hydra.

(setq ivy-read-action-function #'ivy-hydra-read-action)

;; Which key
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;; I also think that having evil- appear in so many popups is a bit too verbose, let’s change that, and do a few other similar tweaks while we’re at it.

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))


(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.4)))
(after! org
  (setq org-ellipsis " ▾ "
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))







(provide 'config)
;;; config.el ends here
