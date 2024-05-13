;;; init.el --- init configuration file -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2023  Vincent Demeester <vincent@sbr.pm>

;; Author: Vincent Demeester <vincent@sbr.pm>
;; URL: https://git.sr.ht/~vdemeester/home
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: https://git.sr.ht/~vdemeester/home

;;; Code:


(defconst emacs-start-time (current-time))

(let ((minver 26))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

;; load early-init.el before Emacs 27.0
(unless (>= emacs-major-version 27)
  (message "Early init: Emacs Version < 27.0")
  (load (expand-file-name "early-init.el" user-emacs-directory)))

(add-to-list 'load-path (concat user-emacs-directory "/lisp/"))

(setq inhibit-default-init t)           ; Disable the site default settings

(setq inhibit-startup-message t
      inhibit-startup-screen t)

(setq confirm-kill-emacs #'y-or-n-p)
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(require 'package)

;; (setq package-archives nil) ;; To rely only on packages from nix
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities
      '(("melpa" .  4)
        ("org" . 3)
        ("gnu" . 2)
	("non-gnu" . 1)))

(require 'tls)

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

;; Init `delight'
(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh")

(defconst vde/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

;; Remove built-in org-mode
(require 'cl-seq)
(setq load-path
      (cl-remove-if
       (lambda (x)
         (string-match-p "org$" x))
       load-path))

(require 'init-func)

(defconst *sys/gui*
  (display-graphic-p)
  "Are we running on a GUI Emacs ?")
(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")
(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")
(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")
(defconst *nix*
  (executable-find "nix")
  "Do we have nix? (aka are we running in NixOS or a system using nixpkgs).")
(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")
(defconst *gcc*
  (executable-find "gcc")
  "Do we have gcc?")
(defconst *git*
  (executable-find "git")
  "Do we have git?")

(defvar *sys/full*
  (member (vde/short-hostname) '("wakasu" "naruhodo")) ; "naruhodo" <- put naruhodo back in
  "Is it a full system ?")
(defvar *sys/light*
  (not *sys/full*)
  "Is it a light system ?")

;; If font-family-mono or font-family-sans are not available, use the default Emacs face
(when (member font-family-mono (font-family-list))
  (set-face-attribute 'default nil
                      :family font-family-mono
                      :height font-height
		      :weight 'semi-light)
  (set-face-attribute 'fixed-pitch nil
                      :family font-family-mono
		      :weight 'medium
		      :height 1.0))
(when (member font-family-sans (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family font-family-sans
                      :weight 'regular))

(vde/el-load-dir (concat user-emacs-directory "/config/"))

(if (file-exists-p (downcase (concat user-emacs-directory "/hosts/" (vde/short-hostname) ".el")))
    (load-file (downcase (concat user-emacs-directory "/hosts/" (vde/short-hostname) ".el"))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
