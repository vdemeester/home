;;; config-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Project related configuration.
;;; This is mainly using projectile now, but built-in projects module seems promising for long-term.
;;; Note: this file is autogenerated from an org-mode file.
;;; Code:

(use-package projectile
  :commands
  (projectile-ack
   projectile-ag
   projectile-compile-project
   projectile-configure-project
   projectile-package-project
   projectile-install-project
   projectile-test-project
   projectile-run-project
   projectile-dired
   projectile-find-dir
   projectile-find-file
   projectile-find-file-dwim
   projectile-find-file-in-directory
   projectile-find-tag
   projectile-test-project
   projectile-grep
   projectile-invalidate-cache
   projectile-kill-buffers
   projectile-multi-occur
   projectile-project-p
   projectile-project-root
   projectile-recentf
   projectile-regenerate-tags
   projectile-replace
   projectile-replace-regexp
   projectile-run-async-shell-command-in-root
   projectile-run-shell-command-in-root
   projectile-switch-project
   projectile-switch-to-buffer
   projectile-vc
   projectile-commander)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq-default projectile-completion-system 'default)
  (setq-default projectile-switch-project-action #'projectile-commander
                projectile-create-missing-test-files t)
  (setq-default compilation-buffer-name-function (lambda (mode) (concat "*" (downcase mode) ": " (projectile-project-name) "*")))
  (setq-default projectile-track-known-projects-automatically nil)
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)
  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project"
    (projectile-run-eshell nil))
  (def-projectile-commander-method ?c
    "Run `compile' in the project"
    (projectile-compile-project nil))
  (defun projectile-ko-project-p ()
    "Check if a project contains a .ko.yaml file."
    (projectile-verify-file ".ko.yaml"))
  (defun projectile-ko-with-config-project-p ()
    "Check if a project is a ko project and has a config/ folder full of yaml"
    (and (projectile-ko-project-p)
         (projectile-verify-file-wildcard "config/*.yaml")))
  (projectile-register-project-type 'ko-with-config #'projectile-ko-with-config-project-p
                                    :project-file ".ko.yaml" ; might not be required
                                    :configure 'projectile-ko-configure-command
                                    :compile 'projectile-ko-compile-command
                                    :test 'projectile-ko-test-command
                                    :run 'projectile-ko-run-command
                                    :package 'projectile-ko-package-command
                                    :install 'projectile-ko-install-command)
  (defun projectile-ko-configure-command ()
    "define a configure command for a ko project, depending on the opened file"
    (cond
     ((projectile-file-exists-p "hack/update-codegen.sh") "./hack/update-codegen.sh")))
  (defun projectile-ko-compile-command ()
    "define a compile command for a ko project, depending on the openend file "
    (cond
     ((eq major-mode 'go-mode) (projectile-ko-compile-command-go))
     ((eq major-mode 'yaml-mode) "yamllint .")
     (t "go build -v ./...")
     ))
  
  (defun projectile-ko-compile-command-go ()
    "compile command for a ko project if in a go file"
    (let* ((current-file (buffer-file-name (current-buffer)))
           (relative-current-file (file-relative-name current-file (projectile-project-root)))
           (relative-current-folder (file-name-directory relative-current-file)))
      (message relative-current-file)
      (cond
       ((string-suffix-p "_test.go" relative-current-file) (format "go test -c -v ./%s" relative-current-folder))
       (t (format "go build -v ./%s" relative-current-folder)))))
  (defun projectile-ko-test-command ()
    "define a test command for a ko project, depending on the openend file"
    (cond
     ((eq major-mode 'go-mode) (projectile-ko-test-command-go))
     (t "go test -v ./...")))
  
  (defun projectile-ko-test-command-go ()
    "test command for a ko project if in a go file"
    (let* ((current-file (buffer-file-name (current-buffer)))
           (relative-current-file (file-relative-name current-file (projectile-project-root)))
           (relative-current-folder (file-name-directory relative-current-file)))
      (cond
       ((string-suffix-p "_test.go" relative-current-file) (projectile-ko-command-go-test relative-current-file))
       (t (format "go test -v ./%s" relative-current-folder)))))
  
  (defun projectile-ko-command-go-test (current-file)
    "get the command for a go test"
    (cond
     ((gotest-module-available-p) (projectile-ko-command-go-test-gotest current-file))
     (t (format "go test -v ./%s" current-file))))
  
  (defun projectile-ko-command-go-test-gotest (current-file)
    "get the command for a go test with gotest module enabled"
    (message default-directory)
    (let ((data (go-test--get-current-file-testing-data)))
      (format "go test -run='%s' -v ./%s" data (file-name-directory current-file))))
  
  (defun gotest-module-available-p ()
    "is go-test module available"
    (fboundp 'go-test--get-current-file-data))
  (defun projectile-ko-run-command ()
    "define a run command for a ko project, depending on the openend file "
    (cond
     ((eq major-mode 'go-mode) (projectile-ko-run-command-go))
     ;; nothing by default ?
     ))
  
  (defun projectile-ko-run-command-go ()
    "test command for a ko project if in a go file"
    (let* ((current-file (buffer-file-name (current-buffer)))
           (relative-current-file (file-relative-name current-file (projectile-project-root)))
           (relative-current-folder (file-name-directory relative-current-file)))
      (cond
       ((string-prefix-p "cmd/" relative-current-file) (format "go run ./%s" relative-current-folder)))))
  
  (defun projectile-ko-package-command ()
    "define a package command for a ko project, depending on the openend file "
    "ko resolve --push=false --oci-layout-path=/tmp/oci -f config")
  (defun projectile-ko-install-command ()
    "define a install command for a ko project, depending on the openend file "
    "ko apply -f config/")
  (projectile-mode))

(provide 'config-projects)
;;; config-projects.el ends here
