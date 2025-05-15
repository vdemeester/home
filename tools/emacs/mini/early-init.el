;; Do not initialize installed packages
(setopt package-enable-at-startup nil
	package-archives nil
	package-quickstart nil)
(setopt use-package-ensure-function 'ignore)

;; Do not resize the frame at this early stage
(setopt frame-inhibit-implied-resize t
	frame-resize-pixelwise t
	frame-title-format '("%b")) ;; do not add "GNU Emacs at …"

;; Disable GUI elements
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setopt use-dialog-box nil   ;; never use dialog-box (no mouse)
	use-file-dialog nil  ;; never use file dialog (gtk)
	use-short-answers t  ;; replace defalias yes-or-no-p
	read-answer-short t) ;; accepts single-character answer, similar to above

(setopt inhibit-startup-message t
	inhibit-startup-screen t
	inhibit-startup-echo-area-message user-login-name ; read the docstring
	inhibit-startup-buffer-menu t)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(defvar vde--file-name-handler-alist file-name-handler-alist)
(defvar vde--vc-handled-backends vc-handled-backends)
(setq file-name-handler-alist nil
      vc-handled-backends nil)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
(setopt inhibit-x-resources t)

;;
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
	debug-on-error t))

;; - Resetting garbage collection and file-name-handler values.
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist vde--file-name-handler-alist
		   vc-handled-backends vde--vc-handled-backends)
             (garbage-collect)) t)
