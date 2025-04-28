;;; config-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configuration of orgmode.
;;; Code:

(use-package s)
(use-package dash)

(defconst org-directory "~/desktop/org/"
  "org-mode directory, where most of the org-mode file lives")
;; P.A.R.A. setup
(defconst org-inbox-file (expand-file-name "20231120T124316--inbox__inbox.org" org-directory)
  "New stuff collected in this file.")
(defconst org-remember-file (expand-file-name "20250311T171330--remember__notes_orgmode_remember.org" org-directory)
  "Remember file, very quick and dirty scratch notes, to be refiled later on.")

(defconst org-archive-dir (expand-file-name "archive" org-directory)
  "Directory of archived files.")

(defconst org-projects-future-file (expand-file-name "20231120T124316--future-projects-incubation__project_future.org" org-directory)
  "Future projects are collected in this file.")
(defconst org-people-dir (expand-file-name "people" org-directory)
  "People files directory.")

(defconst src-home-dir (expand-file-name "~/src/home" org-directory)
  "Directory of my home monorepository, can contain todos there.")
;; 2024-06-11: Should it be in home ? I've been going back and forth on this
(defconst src-www-dir (expand-file-name "~/src/www" org-directory)
  "Directory of my www repository, can contain todos there.")

(defconst org-babel-library-file (expand-file-name "org_library_of_babel.org" org-directory)
  "Org babel library.")

(set-register ?i `(file . ,org-inbox-file))
(set-register ?f `(file . ,org-projects-future-file))
(set-register ?o `(file . ,org-directory))
(set-register ?P `(file . ,org-people-dir))

(defun vde/agenda-goto-view ()
  "Jump to the task narrowed but in view mode only to get a glance."
  (interactive)
  (org-agenda-goto)
  (org-narrow-to-subtree)
  (view-mode t))

(defun vde/org-mode-hook ()
  "Org-mode hook."
  (setq show-trailing-whitespace t)
  (when (not (eq major-mode 'org-agenda-mode))
    (setq fill-column 90)
    (auto-revert-mode 1)
    (auto-fill-mode 1)
    (org-indent-mode 1)
    (visual-line-mode 1)
    (add-hook 'before-save-hook 'org-update-all-dblocks)
    (add-hook 'auto-save-hook 'org-update-all-dblocks)
    (add-hook 'before-save-hook #'save-and-update-includes nil 'make-it-local)))

(use-package org
  :mode (("\\.org$" . org-mode)
         ("\\.org.draft$" . org-mode))
  :commands (org-agenda org-capture)
  :bind (("C-c o l" . org-store-link)
         ("C-c o r r" . org-refile)
	 ("C-c o r R" . vde/reload-org-refile-targets)
         ("C-c o a a" . org-agenda)
	 ("C-c o a r" . vde/reload-org-agenda-files)
	 ("C-c C-x i" . vde/org-clock-in-any-heading)
         ("C-c o s" . org-sort)
	 ("C-c O" . org-open-at-point-global)
         ("<f12>" . org-agenda))
  :hook (org-mode . vde/org-mode-hook)
  :custom
  ;; (org-reverse-note-order '((org-inbox-file . t) ;; Insert items on top of inbox
  ;;                           (".*" . nil)))    ;; On any other file, insert at the bottom
  (org-archive-location (concat org-archive-dir "/%s::datetree/"))
  (org-agenda-file-regexp "^[a-zA-Z0-9-_]+.org$")
  (org-agenda-remove-tags t)
  (org-use-speed-commands t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-return-follows-link t)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "IN-REVIEW(r)" "|" "DONE(d!)" "CANCELED(c@/!)")
                       (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELED(c@/!)")
                       (sequence "IDEA(i)" "|" "CANCELED(c@/!)")))
  (org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                  ("WAITING" ("WAITING" . t))
                                  (done ("WAITING"))
                                  ("TODO" ("WAITING") ("CANCELLED"))
                                  ("NEXT" ("WAITING") ("CANCELLED"))
                                  ("DONE" ("WAITING") ("CANCELLED"))))
  (org-log-done 'time)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-refile-use-cache t)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-skip-scheduled-if-done nil)
  (org-agenda-current-time-string "")
  (org-agenda-time-grid '((daily) () "" ""))
  ;; ((agenda . " %i %-12:c%?-12t% s")
  ;;  (todo . " %i %-12:c")
  ;;  (tags . " %i %-12:c")
  ;;  (search . " %i %-12:c"))
  ;; (org-agenda-prefix-format "   %i %?-2 t%s")
  (org-agenda-prefix-format '((agenda . " %i %?-12t% s")
			      (todo . " %i")
			      (tags . " %i")
			      (search . " %i")))
  (org-insert-heading-respect-content t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-goto-max-level 2)

  (org-agenda-category-icon-alist `(("journal"  ,(list (propertize "📝")))
				    ("project--" ,(list (propertize "💼" )))
				    ("tekton", (list (propertize "😼")))
				    ("openshift-pipelines", (list (propertize "🎩")))
				    ("redhat", (list (propertize "🎩")))
				    ("area--"  ,(list (propertize"🏢" )))
				    ("area--home"  ,(list (propertize"🏡" )))
				    ("home"  ,(list (propertize"🏡" )))
				    ("home-services" ,(list (propertize "☕ ")))
				    ("email"  ,(list (propertize"📨" )))
				    ("people"  ,(list (propertize"👤" )))
				    ("machine" ,(list (propertize "🖥️")))
				    ("website" ,(list (propertize "🌍")))
				    ("bike" ,(list (propertize "🚴‍♂️")))
				    ("security" ,(list (propertize "🛡️")))
				    ("i*" ,(list (propertize "📒")))))
  (org-agenda-sticky t)
  :config

  (defun vde/org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
	(save-excursion (and (looking-at (org-item-re)) (looking-back "^[ \t]*" nil)))))
  (setq org-use-speed-commands 'vde/org-use-speed-commands-for-headings-and-lists)
  ;; TODO: see https://sachachua.com/blog/2025/03/org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command/
  
  ;; Refile org-mode cache when emacs has been idled for 5 minutes
  (run-with-idle-timer 300 t (lambda ()
                               (org-refile-cache-clear)
                               (org-refile-get-targets)))
  
  ;; Org Babel configurations
  (when (file-exists-p org-babel-library-file)
    (org-babel-lob-ingest org-babel-library-file))
  (defun vde/org-agenda-files ()
    (seq-filter (lambda(x) (and (not (string-match "/archive/" (file-name-directory x)))
				(not (string-match ".*==readwise=.*" x))))
		(apply 'append
		       (mapcar
			(lambda (directory)
			  (directory-files-recursively
			   directory org-agenda-file-regexp))
			`(,org-directory)))))
  (defun vde/reload-org-agenda-files ()
    "Reload org-agenda-files variables with up-to-date org files"
    (interactive)
    (setq org-agenda-files (vde/org-agenda-files)))
  (defun vde/reload-org-refile-targets ()
    "Reload org-refile-targets variables with up-to-date org files"
    (interactive)
    (setq org-refile-targets (vde/org-refile-targets)))
  (defun vde/org-refile-targets ()
    (append '((org-inbox-file :level . 0))
	    (->>
	     (directory-files org-directory nil ".org$")
	     (--remove (s-starts-with? "." it))
	     (--map (format "%s/%s" org-directory it))
	     (--map `(,it :maxlevel . 3)))
	    (->>
	     (directory-files-recursively org-people-dir ".org$")
	     (--remove (s-starts-with? (format "%s/legacy" org-people-dir) it))
	     (--map (format "%s" it))
	     (--map `(,it :maxlevel . 3)))))
  (setq org-agenda-files (vde/org-agenda-files)
	;; TODO: extract org-refile-targets into a function
	org-refile-targets (vde/org-refile-targets))
  (setq org-agenda-custom-commands
	'(("d" "Daily Agenda"
	   ((agenda ""
		    ((org-agenda-span 'day)
		     (org-deadline-warning-days 5)))
	    (tags-todo "+PRIORITY=\"A\""
		       ((org-agenda-overriding-header "High Priority Tasks")))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Tasks")))))
	  ("i" "Inbox (triage)"
	   ((tags-todo ".*"
		       ((org-agenda-files '("~/desktop/org/20231120T124316--inbox__inbox.org"))
			(org-agenda-overriding-header "Unprocessed Inbox Item")))))
	  ("u" "Untagged Tasks"
	   ((tags-todo "-{.*}"
		       ((org-agenda-overriding-header "Untagged tasks")))))
	  ("w" "Weekly Review"
	   ((agenda ""
		    ((org-agenda-overriding-header "Completed Tasks")
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
		     (org-agenda-span 'week)))
	    (agenda ""
		    ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
		     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		     (org-agenda-span 'week)))))
	  ;; FIXME Should only take into account projects and areas ?
	  ("R" "Review projects" tags-todo "-CANCELLED/"
           ((org-agenda-overriding-header "Reviews Scheduled")
            (org-agenda-skip-function 'org-review-agenda-skip)
            (org-agenda-cmp-user-defined 'org-review-compare)
	    (org-agenda-sorting-strategy '(user-defined-down)))))))

(use-package org-review
  :defer t
  :after (org)
  :config
  (setopt org-review-delay "+1w")
  (add-hook 'org-agenda-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r")
                           'org-review-insert-last-review))))

;; Make sure we load org-protocol
(use-package org-protocol
  :after org)

(use-package org-tempo
  :after (org)
  :custom
  (org-structure-template-alist '(("a" . "aside")
				  ("c" . "center")
				  ("C" . "comment")
				  ("e" . "example")
				  ("E" . "export")
				  ("Ea" . "export ascii")
				  ("Eh" . "export html")
				  ("El" . "export latex")
				  ("q" . "quote")
				  ("s" . "src")
				  ("se" . "src emacs-lisp")
				  ("sE" . "src emacs-lisp :results value code :lexical t")
				  ("sg" . "src go")
				  ("sr" . "src rust")
				  ("sp" . "src python")
				  ("v" . "verse"))))

(use-package org-id
  :after org
  :commands contrib/org-id-headlines
  :init
  (defun contrib/org-id-headlines ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive)
    (org-map-entries
     (funcall 'contrib/org-get-id (point) 'create)))
  :config
  (setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id))

(use-package org-modern
  ;; :if window-system
  :disabled ;; trying something out.
  :custom (org-modern-table nil)
  :hook (org-mode . org-modern-mode))

(use-package org-capture
  :after org
  :commands (org-capture)
  :config

  ;; TODO: refine this, create a function that reset this
  (add-to-list 'org-capture-templates
               `("l" "Link" entry
                 (file ,org-inbox-file)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
	       `("d" "daily entry" entry
		 (function denote-journal-extras-new-or-existing-entry)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("t" "Tasks"))
  (add-to-list 'org-capture-templates
               `("tt" "New task" entry
                 (file ,org-inbox-file)
                 "* %?\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
                 :empty-lines 1))
  ;; Refine this
  (add-to-list 'org-capture-templates
               `("tr" "PR Review" entry
                 (file ,org-inbox-file)
                 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
                 :empty-lines 1))
  ;; emails
  (add-to-list 'org-capture-templates
	       `("m" "Email Workflow"))
  (add-to-list 'org-capture-templates
	       `("mf" "Follow Up" entry
		(function denote-journal-extras-new-or-existing-entry)
		"* TODO Follow up with %:from on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i"
		:immediate-finish t))
  (add-to-list 'org-capture-templates
	       `("mr" "Read Later" entry
		(function denote-journal-extras-new-or-existing-entry)
		"* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t))
  ;; (add-to-list 'org-capture-templates
  ;;              `("m" "Meeting notes" entry
  ;;                (file+datetree ,org-meeting-notes-file)
  ;;                (file ,(concat user-emacs-directory "/etc/orgmode/meeting-notes.org"))))

  (add-to-list 'org-capture-templates
               `("w" "Writing"))
  (add-hook 'org-capture-after-finalize-hook #'vde/window-delete-popup-frame)
  :bind (("C-c o c" . org-capture)))

(defun vde/dired-notes ()
  "Open a dired buffer with all my notes"
  (interactive)
  (find-dired org-directory "-type f -not -path '*/archive/*'"))

;; Using denote as the "source" of my second brain *in* org-mode.
(use-package denote
  :commands (denote denote-region denote-type denote-date
		    denote-signature denote-subdirectory
		    denote-template denote-link-or-create
		    denote-add-links denote-find-link
		    denote-find-backlink denote-rename-file
		    denote-rename-file-using-front-matter
		    denote-journal-extras-new-or-existing-entry)
  :bind (("C-c n n" . vde/dired-notes)
	 ("C-c n N" . denote)
	 ("C-c n c" . denote-region)
	 ("C-c n N" . denote-type)
	 ("C-c n d" . denote-date)
	 ("C-c n z" . denote-signature)
	 ("C-c n S" . denote-subdirectory)
	 ("C-c n t" . denote-template)
	 ;; Links
	 ("C-c n i" . denote-link-or-create)
	 ("C-c n I" . denote-add-links)
	 ("C-c n b" . denote-backlinks)
	 ("C-c n F f" . denote-find-link)
	 ("C-c n F b" . denote-find-backlink)
	 ;; Renaming
	 ("C-c n r" . denote-rename-file)
	 ("C-c n R" . denote-rename-file-using-front-matter)
	 ;; Journal
	 ("C-c n j j" . denote-journal-extras-new-or-existing-entry)
	 ;; Dired
	 (:map dired-mode-map
	       ("C-c C-d C-i" . denote-link-dired-marked-notes)))
  :custom
  (denote-directory org-directory)
  (denote-rename-buffer-format "📝 %t")
  (denote-date-prompt-denote-date-prompt-use-org-read-date t)
  (denote-prompts '(subdirectory title keywords))
  (denote-journal-extras-directory nil) ;; use denote-directory
  (denote-journal-extras-title-format 'day-date-month-year)
  (denote-backlinks-display-buffer-action
   '((display-buffer-reuse-window
      display-buffer-in-side-window)
     (side . bottom)
     (slot . 99)
     (window-width . 0.3)
     (dedicated . t)
     (preserve-size . (t . t))))
  :hook (dired-mode . denote-dired-mode)
  :config
  (require 'denote-rename-buffer)
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode 1)
  (defun my-denote-always-rename-on-save-based-on-front-matter ()
    "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t)) ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
	(ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
	(message "Buffer saved; Denote file renamed"))))

  (add-hook 'after-save-hook #'my-denote-always-rename-on-save-based-on-front-matter)
  
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  (defun vde/org-category-from-buffer ()
    "Get the org category (#+category:) value from the buffer"
    (cond
     ((string-match "__journal.org$" (buffer-file-name))
      "journal")
     (t
      (denote-sluggify (denote--retrieve-title-or-filename (buffer-file-name) 'org))))))

(use-package denote-menu
  :after denote
  :bind (("C-c n m" . list-denotes)
	 (:map denote-menu-mode-map
	       ("c" . denote-menu-clear-filters)
	       ("/ r" . denote-menu-filter)
	       ("/ k" . denote-menu-filter-by-keyword)
	       ("/ o" . denote-menu-filter-out-keyword)
	       ("e" . denote-menu-export-to-dired))))

(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes
	     consult-notes-denote-mode)
  :bind (("C-c n f" . consult-notes)
	 ("C-c n s" . consult-notes-search-in-all-notes))
  :config
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package orgit
  :defer t)

(use-package ob-async
  :after org
  :commands (ob-async-org-babel-execute-src-block))
(use-package ob-emacs-lisp
  :after org
  :commands (org-babel-execute:emacs-lisp org-babel-execute:elisp))
(use-package ob-go
  :after org
  :commands (org-babel-execute:go))
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
;; my personal
(use-package ol-github
  :after (org))
(use-package ol-gitlab
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
;; (use-package ol-notmuch
;;   :defer 2
;;   :after (org))
;; (use-package ob-dot
;;   :after org
;;   :commands (org-babel-execute:dot))
;; (use-package ob-ditaa
;;   :after org
;;   :commands (org-babel-execute:ditaa)
;;   :config
;;   (setq org-ditaa-jar-path "/home/vincent/.nix-profile/lib/ditaa.jar"))
;; (use-package ob-doc-makefile
;;   :after org
;;   :commands (org-babel-execute:makefile))

(use-package org-nix-shell
  :hook (org-mode . org-nix-shell-mode))

(use-package org-rich-yank
  :after org
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

;; from https://sachachua.com/blog/2024/01/using-consult-and-org-ql-to-search-my-org-mode-agenda-files-and-sort-the-results-to-prioritize-heading-matches/
(defun my-consult-org-ql-agenda-jump ()
  "Search agenda files with preview."
  (interactive)
  (let* ((marker (consult--read
                  (consult--dynamic-collection
                   #'my-consult-org-ql-agenda-match)
                  :state (consult--jump-state)
                  :category 'consult-org-heading
                  :prompt "Heading: "
                  :sort nil
                  :lookup #'consult--lookup-candidate))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    ;; based on org-agenda-switch-to
    (unless buffer (user-error "Trying to switch to non-existent buffer"))
    (pop-to-buffer-same-window buffer)
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (org-fold-show-context 'agenda)
      (run-hooks 'org-agenda-after-show-hook))))

(defun my-consult-org-ql-agenda-format (o)
  (propertize
   (org-ql-view--format-element o)
   'consult--candidate (org-element-property :org-hd-marker o)))

(defun my-consult-org-ql-agenda-match (string)
  "Return candidates that match STRING.
Sort heading matches first, followed by other matches.
Within those groups, sort by date and priority."
  (let* ((query (org-ql--query-string-to-sexp string))
         (sort '(date reverse priority))
         (heading-query (-tree-map (lambda (x) (if (eq x 'rifle) 'heading x)) query))
         (matched-heading
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files heading-query
				 :action 'element-with-markers
				 :sort sort)))
         (all-matches
          (mapcar #'my-consult-org-ql-agenda-format
                  (org-ql-select 'org-agenda-files query
				 :action 'element-with-markers
				 :sort sort))))
    (append
     matched-heading
     (seq-difference all-matches matched-heading))))

(use-package org-ql
  :after org
  :bind ("M-s a" . my-consult-org-ql-agenda-jump))

(use-package org-ql-view
  :after org-ql)


;; TODO NEXT STARTED IN-REVIEW DONE CANCELED WAITING SOMEDAY IDEA
(defun my-org-todo-set-keyword-faces ()
  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground ,(modus-themes-get-color-value 'red-faint) :weight bold))
          ("NEXT" . (:foreground ,(modus-themes-get-color-value 'yellow-warmer) :weight bold))
          ("STARTED" . (:foreground ,(modus-themes-get-color-value 'yellow-intense) :weight bold))
          ("IN-REVIEW" . (:foreground ,(modus-themes-get-color-value 'blue-faint) :weight bold))
          ("DONE" . (:foreground ,(modus-themes-get-color-value 'green-warmer) :weight bold))
          ("CANCELED" . (:foreground ,(modus-themes-get-color-value 'comment) :weight bold))
          ("WAITING" . (:foreground ,(modus-themes-get-color-value 'magenta-faint) :weight bold))
          ("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'cyan-warmer) :weight bold))
          ("IDEA" . (:foreground ,(modus-themes-get-color-value 'magenta-cooler) :weight bold))))
  (setq org-modern-todo-faces
        `(("TODO" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'red-faint) :weight bold))
          ("NEXT" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'yellow-warmer) :weight bold))
          ("STARTED" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'yellow-intense) :weight bold))
          ("IN-REVIEW" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'blue-faint) :weight bold))
          ("DONE" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'green-warmer) :weight bold))
          ("CANCELED" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'comment) :weight bold))
          ("WAITING" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'magenta-faint) :weight bold))
          ("SOMEDAY" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'cyan-warmer) :weight bold))
          ("IDEA" . (:foreground ,(modus-themes-get-color-value 'fg-term-white-bright) :background ,(modus-themes-get-color-value 'magenta-cooler) :weight bold))))
  (when (derived-mode-p 'org-mode)
    (font-lock-fontify-buffer)))
(my-org-todo-set-keyword-faces)
(with-eval-after-load 'modus-themes
  (add-hook 'modus-themes-after-load-theme-hook #'my-org-todo-set-keyword-faces))

(use-package ox-publish
  :after org
  :commands (org-publish org-publish-all org-publish-project org-publish-current-project org-publish-current-file)
  :config
  (setq org-html-coding-system 'utf-8-unix
	org-publish-use-timestamps-flag nil)
  (defun vde-org-git-exportable-files (directory)
    "Return a list of files from `DIRECTORY' that can be exported."
    (directory-files directory nil ".*_www.*\\.org$"))
  (setq org-publish-project-alist
	`(("resources"
	   :base-directory ,org-directory
	   :base-extension "org"
	   ;; :include ".*_www*.org"
	   :include ,(vde-org-git-exportable-files org-directory)
	   :exclude ".*"
	   :publishing-directory ,(expand-file-name "resources" src-www-dir)
	   :publishing-function org-html-publish-to-html
	   :recursive t
	   :with-toc nil
	   :section-numbers nil
	   :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/2022.css\" />"
	   :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"../css/syntax.css\" />"
	   :html-preamble t
	   :html-postamble t
	   :auto-sitemap t
	   :sitemap-filename "index.org"
	   :sitemap-title "Resources"
	   :sitemap-sort-files anti-chronologically
	   :sitemap-file-entry-format "%d %t"
	   :sitemap-date-format "%Y-%m-%d"
	   ;; :sitemap-function org-publish-org-sitemap
	   ))))
(use-package org-habit
  :after (org)
  :config
  (setq org-habit-show-habits-only-for-today nil
        org-habit-graph-column 80))
(use-package org-download
  :after (org)
  :hook ((dired-mode . org-download-enable)
         (org-mode . org-download-enable))
  :config
  (org-download-enable)
  (setq org-startup-with-inline-images t)
  (setq org-download-display-inline-images t)
  (setq org-download-method 'attach))

;; Persistent notes (like persistent-scratch, but built-in)
(setq remember-data-file org-remember-file
      remember-handler-functions '(remember-append-to-file)
      remember-notes-initial-major-mode 'org-mode
      remember-notes-auto-save-visited-file-name t
      remember-in-new-frame t)

(use-package consult-org
  :after (consult org)
  :commands (consult-org-agenda consult-org-heading))

(provide 'config-org)
;;; config-org.el ends here
