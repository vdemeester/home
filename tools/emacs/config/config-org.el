;;; config-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration of orgmode.
;;; Code:

(use-package s)

(defconst org-directory "~/desktop/org/"
  "org-mode directory, where most of the org-mode file lives")
(defconst org-projects-dir (expand-file-name "projects" org-directory)
  "Primary tasks directory.")
(defconst org-notes-dir (expand-file-name "notes" org-directory)
  "Directory of shareable, technical notes.")
(defconst org-archive-dir (expand-file-name "archive" org-directory)
  "Directory of shareable, technical notes.")
(defconst org-completed-dir (expand-file-name "projects" org-archive-dir)
  "Directory of completed project files.")
(defconst org-inbox-file (expand-file-name "inbox.org" org-projects-dir)
  "New stuff collected in this file.")
(defconst org-next-file (expand-file-name "next.org" org-projects-dir)
  "Todo *next* collected in this file.")
(defconst org-incubate-file (expand-file-name "incubate.org" org-projects-dir)
  "Ideas simmering on back burner.")
(defconst org-babel-library-file (expand-file-name "org_library_of_babel.org" org-notes-dir)
  "Org babel library.")
(set-register ?i `(file . ,org-inbox-file))
(set-register ?I `(file . ,org-incubate-file))
(set-register ?n `(file . ,org-next-file))
(use-package org
  ;; :ensure org-plus-contrib ;; load from the package instead of internal
  :mode (("\\.org$" . org-mode)
         ("\\.org.draft$" . org-mode))
  :commands (org-agenda org-capture)
  :bind (("C-c o l" . org-store-link)
         ("C-c o r r" . org-refile)
         ("C-c o a a" . org-agenda)
         ("C-c o a r" . my/reload-org-agenda-files)
         ("C-c o s" . org-sort)
         ("<f12>" . org-agenda)
         ("C-c o c" . org-capture)
         ;; Skeletons
         ("C-c o i p" . vde/org-project)
         ("C-c o i n" . vde/org-www-post))
  :config
  (define-skeleton vde/org-project
    "new org-mode project"
    nil
    > "#+TITLE: " (skeleton-read "Title: ") \n
    > "#+FILETAGS: " (skeleton-read "Tags: ") \n
    > "#+CATEGORY: " (skeleton-read "Category: ") \n
    > _ \n
    > _ \n)
  (define-auto-insert '("/projects/.*\\.org\\'" . "projects org files") [vde/org-project])
  (define-skeleton vde/org-www-post
    "new www post"
    nil
    > "#+title: " (skeleton-read "Title: ") \n
    > "#+date: " (format-time-string "<%Y-%m-%d %a>") \n
    > "#+filetags: " (skeleton-read "Tags: ") \n
    > "#+setupfile: ../templates/post.org" \n
    > _ \n
    > "* Introduction"
    )
  (define-auto-insert '("/posts/.*\\.org\\'" . "blog post org files") [vde/org-www-post])
  (define-auto-insert '("/posts/.*\\.draft\\'" . "blog post draft files") [vde/org-www-post])
  ;; Org Babel configurations
  (when (file-exists-p org-babel-library-file)
    (org-babel-lob-ingest org-babel-library-file))
  (defun my/org-agenda-files ()
    `(,org-projects-dir
      ,org-notes-dir))
  (defun my/reload-org-agenda-files ()
    (interactive)
    (setq org-agenda-files (my/org-agenda-files)))
  (setq org-agenda-files (my/org-agenda-files)
        org-agenda-file-regexp "^[a-zA-Z0-9-_]+.org$"
        org-use-speed-commands t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "|" "DONE(d!)" "CANCELED(c@/!)")
                            (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELED(c@/!)")
                            (sequence "IDEA(i)" "|" "CANCELED(c@/!)"))
        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       (done ("WAITING"))
                                       ("TODO" ("WAITING") ("CANCELLED"))
                                       ("NEXT" ("WAITING") ("CANCELLED"))
                                       ("DONE" ("WAITING") ("CANCELLED")))
        org-use-tag-inheritance t
        org-tag-alist '(("linux") ("nixos") ("emacs") ("org")
                        ("openshift") ("redhat") ("tektoncd") ("kubernetes") ("knative" ) ("docker")
                        ("docs") ("code") ("review")
                        (:startgroup . nil)
                        ("#home" . ?h) ("#work" . ?w) ("#errand" . ?e) ("#health" . ?l)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("#link" . ?i) ("#read" . ?r) ("#project" . ?p)
                        (:endgroup . nil))
        org-log-done 'time
        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-into-drawer t
        org-enforce-todo-dependencies t
        org-refile-targets (append '((org-inbox-file :level . 0))
                                   (->>
                                    (directory-files org-projects-dir nil ".org")
                                    (--remove (s-starts-with? "." it))
                                    (--map (format "%s/%s" org-projects-dir it))
                                    (--map `(,it :level . 1))))
        org-refile-use-outline-path 'file
        org-refile-allow-creating-parent-nodes 'confirm
        org-outline-path-complete-in-steps nil
        org-columns-default-format "%80ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-fontify-whole-heading-line t
        org-pretty-entities t
        org-ellipsis " ⤵"
        org-archive-location (concat org-completed-dir "/%s::datetree/")
        org-use-property-inheritance t
        org-priority 67
        org-priority-faces '((?A . "#ff2600")
                             (?B . "#ff5900")
                             (?C . "#ff9200")
                             (?D . "#747474"))
        org-global-properties (quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-insert-heading-respect-content t
        org-yank-adjusted-subtrees t
        org-image-actual-width nil
        org-startup-with-inline-images nil
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))
        org-catch-invisible-edits 'error
        ;; Put theses into a minor mode
        org-indent-indentation-per-level 1
        org-cycle-separator-lines 1
        org-adapt-indentation nil
        org-hide-leading-stars t
        org-hide-emphasis-markers nil)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)
  :hook (org-mode . vde/org-mode-hook))

(defun vde/org-mode-hook ()
  "Org-mode hook"
  (setq show-trailing-whitespace t)
  (when (not (eq major-mode 'org-agenda-mode))
    (setq fill-column 90)
    (auto-revert-mode)
    (auto-fill-mode)
    (org-indent-mode)
    (add-hook 'before-save-hook #'save-and-update-includes nil 'make-it-local)))
(use-package org-agenda
  :after org
  :commands (org-agenda)
  :bind (("C-c o a a" . org-agenda)
         ("<f12>" . org-agenda)
         ("C-c o r a" . org-agenda-refile))
  :config
  (use-package org-super-agenda
    :config (org-super-agenda-mode))
  (setq org-agenda-span 'day
        org-agenda-start-on-weekday 1
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-agenda-skip-scheduled-if-done nil
        org-agenda-compact-blocks t
        org-agenda-sticky t
        org-super-agenda-header-separator ""
        org-agenda-custom-commands
        `(("l" "Links"
           tags "+#link")
          ("w" "Agenda"
           ((agenda "")
            (tags-todo "-goals-incubate-inbox+TODO=\"STARTED\""
                       ((org-agenda-overriding-header "Ongoing")))
            (tags-todo "-goals-incubate-inbox+TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Next"))))
           ((org-super-agenda-groups
             '((:name "Important" :priority "A")
               (:name "Scheduled" :time-grid t)
               (:habit t))))
           (org-agenda-list)))))
(use-package org-capture
  :after org
  :commands (org-capture)
  :config

  (add-to-list 'org-capture-templates
               `("l" "Link" entry
                 (file ,org-inbox-file)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))

  (add-to-list 'org-capture-templates
               `("t" "Tasks"))
  (add-to-list 'org-capture-templates
               `("tt" "New task" entry
                 (file ,org-inbox-file)
                 "* %?\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("tr" "PR Review" entry
                 (file ,org-inbox-file)
                 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
                 :empty-lines 1))

  ;; (add-to-list 'org-capture-templates
  ;;              `("m" "Meeting notes" entry
  ;;                (file+datetree ,org-meeting-notes-file)
  ;;                (file ,(concat user-emacs-directory "/etc/orgmode/meeting-notes.org"))))

  (add-to-list 'org-capture-templates
               `("w" "Writing"))
  :bind (("C-c o c" . org-capture)))


(use-package org-capture-pop-frame
  :after org)
(use-package org-protocol
  :after org)
(use-package org-clock
  :after org
  :commands (org-clock-in org-clock-out org-clock-goto)
  :config
  ;; Setup hooks for clock persistance
  (org-clock-persistence-insinuate)
  (setq org-clock-clocked-in-display nil
        ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;; Change tasks to STARTED when clocking in
        org-clock-in-switch-to-state 'vde/clock-in-to-started
        ;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t)
  (use-package find-lisp)
  (defun vde/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun vde/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (vde/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun vde/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun vde/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun vde/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun vde/clock-in-to-started (kw)
    "Switch a task from TODO to STARTED when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from STARTED back to TODO"
    (when (not (and (boundp 'org-capture-mode) org-capture-mode))
      (cond
       ((and (member (org-get-todo-state) (list "TODO" "NEXT"))
             (vde/is-task-p))
        "STARTED")
       ((and (member (org-get-todo-state) (list "STARTED"))
             (vde/is-project-p))
        "TODO"))))
  :bind (("<f11>" . org-clock-goto)))
(use-package org-habit
  :after (org)
  :config
  (setq org-habit-show-habits-only-for-today nil
        org-habit-graph-column 80))
(use-package org-src
  :after (org)
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-edit-src-content-indentation 0))
;; my personal
(use-package ol-github
  :after (org))
(use-package ol-gitlab
  :after (org))
(use-package ol-ripgrep
  :after (org))
(use-package ol-rg
  :disabled
  :after (org))
(use-package ol-grep
  :after (org))

;; built-in org-mode
(use-package ol-eshell
  :after (org))
(use-package ol-git-link
  :defer 2
  :after (org))
(use-package ol-gnus
  :defer 2
  :after (org))
(use-package ol-irc
  :defer 2
  :after (org))
(use-package ol-info
  :defer 2
  :after (org))
(use-package ol-man
  :defer 2
  :after (org))
(use-package ol-notmuch
  :defer 2
  :after (org))
(use-package orgit
  :after org)
(use-package ob-async
  :after org
  :commands (ob-async-org-babel-execute-src-block))
(use-package ob-css
  :after org
  :commands (org-babel-execute:css))
(use-package ob-dot
  :after org
  :commands (org-babel-execute:dot))
(use-package ob-ditaa
  :after org
  :commands (org-babel-execute:ditaa)
  :config
  (setq org-ditaa-jar-path "/home/vincent/.nix-profile/lib/ditaa.jar"))
(use-package ob-emacs-lisp
  :after org
  :commands (org-babel-execute:emacs-lisp org-babel-execute:elisp))
(use-package ob-go
  :after org
  :commands (org-babel-execute:go))
(use-package ob-gnuplot
  :after org
  :commands (org-babel-execute:gnuplot))
(use-package ob-http
  :after org
  :commands (org-babel-execute:http))
(use-package ob-js
  :after org
  :commands (org-babel-execute:js))
(use-package ob-latex
  :after org
  :commands (org-babel-execute:latex))
(use-package ob-python
  :after org
  :commands (org-babel-execute:python))
(use-package ob-shell
  :after org
  :commands (org-babel-execute:ash
             org-babel-execute:bash
             org-babel-execute:csh
             org-babel-execute:dash
             org-babel-execute:fish
             org-babel-execute:ksh
             org-babel-execute:mksh
             org-babel-execute:posh
             org-babel-execute:sh
             org-babel-execute:shell
             org-babel-execute:zsh))
(use-package ob-doc-makefile
  :after org
  :commands (org-babel-execute:makefile))
(use-package org
  :defer 2
  :config
  (defun vde/tangle-all-notes ()
    "Produce files from my notes folder.
This function will attempt to tangle all org files from `org-notes-dir'. The
assumption is that those will generate configuration file (in `~/src/home'),
and thus keeping the configuration source up-to-date"
    (mapc (lambda (x) (org-babel-tangle-file x))
          (ignore-errors
            (directory-files-recursively org-notes-dir "\.org$")))))
(use-package org-journal
  :commands (org-journal-new-entry org-capture)
  :after (org-capture)
  :bind
  (("C-c n j" . org-journal-new-entry)
   ("C-c o j" . org-journal-new-entry))
  :init
  (defun org-journal-find-location ()
    "Go to the beginning of the today's journal file.

This can be used for an org-capture template to create an entry in the journal."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (widen)
    (goto-char (point-min))
    (org-show-entry))
  (add-to-list 'org-capture-templates
               `("j" "Journal"))
  (add-to-list 'org-capture-templates
               `("jj" "Journal entry" entry (function org-journal-find-location)
                 "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("je" "Weekly review" entry (function org-journal-find-location)
                 (file ,(expand-file-name "etc/orgmode/weekly.org" user-emacs-directory))
                 :empty-lines 1 :clock-in t :clock-resume t))
  :custom
  (org-journal-date-prefix "* ")
  (org-journal-file-header "#+TITLE: %Y-v%m Journal\n\n")
  (org-journal-file-format "%Y-%m.private.org")
  (org-journal-file-type 'monthly)
  (org-journal-dir org-notes-dir)
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-enable-agenda-integration nil))
(use-package org-id
  :after org
  :commands contrib/org-id-headlines
  :config
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

  (defun contrib/org-get-id (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker
POM. If POM is nil, refer to the entry at point. If the entry
does not have an CUSTOM_ID, the function returns nil. However,
when CREATE is non nil, create a CUSTOM_ID if none is present
already. PREFIX will be passed through to `org-id-new'. In any
case, the CUSTOM_ID of the entry is returned."
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun contrib/org-id-headlines ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive)
    (org-map-entries
     (funcall 'contrib/org-get-id (point) 'create))))
(use-package org-crypt
  :after (org)
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")))
(use-package org-tempo
  :after (org))
(use-package org-attach
  :after org
  :config
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link))))
(use-package ox-publish
  :after org
  :commands (org-publish org-publish-all org-publish-project org-publish-current-project org-publish-current-file)
  :config
  (setq org-html-coding-system 'utf-8-unix))
(use-package diary-lib
  :after (org)
  :config
  (setq diary-entry-marker "diary")
  (setq diary-show-holidays-flag t)
  (setq diary-header-line-flag nil)
  (setq diary-mail-days 3)
  (setq diary-number-of-entries 3)
  (setq diary-comment-start ";")
  (setq diary-comment-end "")
  (setq diary-date-forms
        '((day "/" month "[^/0-9]")
          (day "/" month "/" year "[^0-9]")
          (day " *" monthname " *" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (year "[-/]" month "[-/]" day "[^0-9]")
          (dayname "\\W"))))

(use-package org
  :defer t
  :config

  (defvar org-capture-templates (list))
  (setq org-protocol-default-template-key "l")

  ;; images
  (setq org-image-actual-width nil
        org-startup-with-inline-images nil)

  ;; Tasks (-> inbox)

  ;; Journal

  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" "#\\+END_EXAMPLE"))

  ;; org-links
  ;; from http://endlessparentheses.com/use-org-mode-links-for-absolutely-anything.html
  (org-link-set-parameters "tag"
                           :follow #'endless/follow-tag-link)
  (defun endless/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" org-directory))
   :follow   (lambda (link) (find-file (expand-file-name link org-directory)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link org-directory))
                   'org-link
                 'error)))
  (defun +org-link-read-file (key dir)
    (let ((file (read-file-name (format "%s: " (capitalize key)) dir)))
      (format "%s:%s"
              key
              (file-relative-name file dir))))
  )

(provide 'config-org)
;;; config-org.el ends here
