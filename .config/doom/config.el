;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;;; Code:
(setq!
 user-full-name "Damien Ayers"
 user-mail-address "damien@omad.net"

 ;; doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14)
 ;; doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 20)
 ;; doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
 doom-scratch-initial-major-mode 'lisp-interaction-mode
 projectile-project-search-path '("~/dev/")
 auto-save-visited-mode t
 auto-save-default t ; Turn on Automatic Saves
 calendar-date-style 'european ; American date format is the worst
 ispell-dictionary "en_AU"
 ispell-personal-dictionary (concat doom-private-dir "my-dictionary.pws")
 ws-butler-keep-whitespace-before-point t
 grip-update-after-change nil  ; Markdown previews after save, not after change
 lsp-pyright-venv-path (expand-file-name "~/miniconda3/envs/"))



;; (add-to-list 'org-pandoc-menu-entry '(58 "to rst." org-pandoc-export-to-rst))
;; (add-to-list 'org-pandoc-menu-entry '(42 "to rst and open." org-pandoc-export-to-rst-and-open))
;(setq org-pandoc-menu-entry '((58 "to rst." org-pandoc-export-to-rst)
;                              (42 "to rst and open." org-pandoc-export-to-rst-and-open))

;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '(
                                  (:name "Today"
                                   :time-grid t
                                   :scheduled today))
        org-super-agenda-header-map nil)

  (org-super-agenda-mode))

(defun dra/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-span 'week
      org-agenda-todo-list-sublevels nil
                                        ; Thanks https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
      org-agenda-custom-commands
      '(("d" "Damien's Daily"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-start-day nil)
                      (org-agenda-span 'day)))
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (dra/org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))))))
                                        ;          (tags-todo "+PRIORITY=\"B\""))
;         (org-agenda-sorting-strategy '(priority-down))
;         (org-agenda-start-day nil)
;         (org-agenda-span 'day)))) ; only list top level TODOs

(use-package projectile-git-autofetch
  :after projectile magit
  :config
  (projectile-git-autofetch-mode 1)
  )
(setq sql-postgres-login-params
      '((user :default "dra547")
        (database :default "datacube")
        (server :default "localhost")
        ))
(setq sql-connection-alist
      '((dea-db (sql-product 'postgres)
                (sql-port 15432)
                (sql-user "dra547")
                (sql-server "localhost")
                (sql-database "datacube"))
        (deadev (sql-product 'postgres)
                (sql-port 15433)
                (sql-user "dra547")
                (sql-server "localhost")
                (sql-database "datacube"))
        (deaprod (sql-product 'postgres)
                 (sql-port 15434)
                 (sql-user "dra547")
                 (sql-server "localhost")
                 (sql-database "datacube"))))
(setq sql-postgres-options '("-P" "pager=off" "--no-psqlrc"))


(setq doom-theme 'doom-vibrant)

(use-package! xref-rst
  :when (featurep! :tools lookup)
  ;;   :after rst-mode
  :hook (rst-mode . xref-rst-mode)
  :init
  (set-lookup-handlers! '(rst-mode sphinx-mode)
    :references #'+lookup-xref-definitions-backend-fn
    :definition #'+lookup-xref-definitions-backend-fn
    :xref-backend #'xref-rst-xref-backend))


(remove-hook! text-mode
  #'display-line-numbers-mode)

(global-subword-mode 1)  ; iterate through CamelCase words

(setq!
 org-journal-enable-agenda-integration t
 org-journal-file-type 'weekly
 org-journal-file-format "%Y-%m-%d.org")
;; org-journal-file-header "#+TITLE: Weekly Journal\n#+STARTUP: folded"
;; org-journal-date-format "%A, %d %B %Y")


;; (map! :leader :desc "Today's Entry" "nj." (cmd! (org-journal-open-current-journal-file)))
(map! :leader :desc "Today's Entry" "nj." #'org-journal-open-current-journal-file)


(after! python
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3")))

(setq! auth-sources '("secrets:Login" "~/.authinfo.gpg" "~/.authinfo"))

;;(when IS-LINUX
;;  (font-put doom-font :weight 'semi-light))

;;(when IS-MAC
;;  (setq ns-use-thin-smoothing t))


;; Have treemacs follow the currently open file
(add-hook! 'treemacs-mode #'treemacs-follow-mode)

;; With some creative use of X401 and xrandr, this finally works in Windows
;(add-hook! 'window-setup-hook #'toggle-frame-maximized)

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
                 (tramp-login-program "env")
                 (tramp-login-args (("SHELL=/bin/sh" "yadm" "enter")))
                 ;; (tramp-login-program "yadm")
                 ;; (tramp-login-args (("enter")))
                 ;; (tramp-login-env (("SHELL") ("/bin/sh")))
                 )))

(map! :leader :desc "yadm status" "g." (cmd! (magit-status "/yadm::")))


(set-popup-rule! "^\\*eww\\*" :ignore t)

(add-hook! markdown-mode
           'auto-fill-mode)

(after! magit
  (setq!
   magit-repository-directories '(("~/PycharmProjects/" . 1) ("~/dev/" . 1))))


(after! org
  (custom-set-faces!
    '(org-document-title :height 1.4))
  (setq!
   org-log-done 'time
   org-log-into-drawer t
   org-directory "~/org/"
   org-agenda-files '("~/org/tasks.org"
                      "~/org/todo.org"
                      "~/org/inbox.org"
                      "~/org/refile.org"
                      "~/org/projects.org"
                      "~/org/work_todo.org"
                      "~/org/work-calendar.org"
                      "~/org/goog-cal.org")
   org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
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
   org-roam-directory "~/org"
   org-hide-emphasis-markers t
   org-todo-keywords '((type "TODO(t!)" "NEXT(w)" "WIP(w!)" "CHASE(c!)" "GAVE(g!)" "|" "DONE(d!)" "KILL(k@/!)"))
   org-pretty-entities t
   org-export-with-toc nil
   org-src-preserve-indentation t
   org-blank-before-new-entry '((heading) (plain-list-item))
   org-capture-templates '(
                           ("t" "Todo [inbox]" entry
                            (file+headline "inbox.org" "Tasks")
                            "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:\n  %i\n  %a\n")
                                        ;                           ("t" "Todo" entry (function org-journal-find-location)
                                        ;                            "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:"
                                        ;                            :empty-lines-before 1
                           ("j" "Journal entry" plain
                            (function org-journal-find-location)
                            "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                            :jump-to-captured t
                            :immediate-finish t)
                           ("m" "Meeting [refile]" entry
                            (file "refile.org")
                            "* MEETING: %? \n:MEETING:\n%U\n%a"
                            :clock-in t
                            :clock-resume t)
                           ("w" "Work Todo" entry
                            (file "work_todo.org")
                            "* TODO %?\n:LOGBOOK:\n- Added: %U\n:END:\n%a\n%i"
                            :prepend t
                            :clock-in t
                            :clock-resume t))

   ;; *****************************************
   ;; Refiling
   ;; *****************************************
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
   org-ellipsis " ▾ "
   org-priority-highest ?A
   org-priority-lowest ?E
   org-priority-faces '((?A . 'all-the-icons-red)
                        (?B . 'all-the-icons-orange)
                        (?C . 'all-the-icons-yellow)
                        (?D . 'all-the-icons-green)
                        (?E . 'all-the-icons-blue))))

(add-hook! org-mode
           'auto-fill-mode
           'eldoc-mode
           ;; 'hide-mode-line-mode
           'doom-disable-delete-trailing-whitespace-h)


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

;; This has bugs!
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

;; Turn off hard line wraps in markdown and GFM
(add-hook! (gfm-mode markdown-mode) #'visual-line-mode #'turn-off-auto-fill)


(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

;; While in an ivy mini-buffer C-o shows a list of all possible actions one may
;; take. By default this is #'ivy-read-action-by-key however a better interface
;; to this is using Hydra.

(setq ivy-read-action-function #'ivy-hydra-read-action)

;; Which key
(setq which-key-idle-delay 0.2) ;; I need the help, I really do

;; I also think that having evil- appear in so many popups is a bit too verbose,
;; let’s change that, and do a few other similar tweaks while we’re at it.

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))))


;; Workaround for Doom+org-capture bug https://github.com/hlissner/doom-emacs/issues/4832
(advice-add #'org-capture :around
            (lambda (fun &rest args)
              (letf! ((#'+org--restart-mode-h #'ignore))
                (apply fun args))))


(defun dra/pomodoro-str ()

  (if (boundp 'org-pomodoro-state)
      (let ((s (cl-case org-pomodoro-state
                 (:none "No Pomodoro")
                 (:pomodoro "Active: %s")
                 (:overtime org-pomodoro-overtime-format)
                 (:short-break org-pomodoro-short-break-format)
                 (:long-break org-pomodoro-long-break-format))))

        (format s (org-pomodoro-format-seconds)))
    "Org Pomodoro not loaded"))

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))







(provide 'config)
;;; config.el ends here
