;;; early-init.el --- Early init configuration file -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2021  Vincent Demeester <vincent@sbr.pm>

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

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.
;;

;; See my dotfiles: https://git.sr.ht/~vdemeester/home

;;; Code:

;; Do not initialize installed packages
(setq package-enable-at-startup nil)

;; Do not resize the frame at this early stage
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar contrib/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun contrib/run-after-load-theme-hook (&rest _)
  "Run `contrib/after-load-theme-hook'."
  (run-hooks 'contrib/after-load-theme-hook))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(advice-add #'load-theme :after #'contrib/run-after-load-theme-hook)
(require 'modus-themes)

(defun vde/modus-operandi ()
  "Enable some Modus Operandi variables and load the theme.
This is used internally by `vde/modus-themes-toggle'."
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-subtle-diffs t
        modus-themes-rainbow-headings t
        modus-themes-section-headings nil
        modus-themes-scale-headings nil
        modus-themes-fringes 'subtle ; {nil,'subtle,'intense}
        modus-themes-mode-line '(moody)
        modus-themes-hl-line nil
        modus-themes-intense-paren-match t
        modus-themes-prompts '(subtle-accented) ; {nil,'subtle,'intense}
        modus-themes-completions '(opinionated) ; {nil,'moderate,'opinionated}
        modus-themes-diffs 'desaturated ; {nil,'desaturated,'fg-only}
        modus-themes-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-themes-links '(neutral-underline)
        modus-themes-paren-match '(subtle-bold)
        modus-themes-syntax nil
        modus-themes-variable-pitch-headings nil
        modus-themes-rainbow-headings t
        modus-themes-section-headings nil
        modus-themes-scale-headings t
        modus-themes-scale-1 1.05
        modus-themes-scale-2 1.1
        modus-themes-scale-3 1.15
        modus-themes-scale-4 1.2
        x-underline-at-descent-line t)
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(defun vde/modus-operandi-custom ()
  "Customize modus-operandi theme."
  (modus-themes-with-colors
    (custom-set-faces
     `(whitespace-tab ((,class (:background "#ffffff" :foreground "#cccccc"))))
     `(whitespace-space ((,class (:background "#ffffff" :foreground "#cccccc"))))
     `(whitespace-hspace ((,class (:background "#ffffff" :foreground "#cccccc"))))
     `(whitespace-newline ((,class (:background "#ffffff" :foreground "#cccccc"))))
     `(whitespace-indentation ((,class (:background "#ffffff" :foreground "#cccccc"))))
     )))

(add-hook 'modus-themes-after-load-theme-hook #'vde/modus-operandi-custom)
(vde/modus-operandi)

(defconst font-height 130
  "Default font-height to use.")
(defconst font-family-mono "Ubuntu Mono"
  "Default monospace font-family to use.")
(defconst font-family-sans "Ubuntu Sans"
  "Default sans font-family to use.")
;; Middle/Near East: שלום, السّلام عليكم
(when (member "Noto Sans Arabic" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Arabic"))
(when (member "Noto Sans Hebrew" (font-family-list))
  (set-fontset-font t 'arabic "Noto Sans Hebrew"))
;; Africa: ሠላም
(when (member "Noto Sans Ethiopic" (font-family-list))
  (set-fontset-font t 'ethiopic "Noto Sans Ethiopic"))

;; If font-family-mono or font-family-sans are not available, use the default Emacs face
(when (member font-family-mono (font-family-list))
  (set-face-attribute 'default nil
                      :family font-family-mono
                      :height font-height)
  (set-face-attribute 'fixed-pitch nil
                      :family font-family-mono))
(when (member font-family-sans (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family font-family-sans
                      :weight 'regular))

(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; - Reseting garbage collection and file-name-handler values.
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)
