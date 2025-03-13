;;; config-keybindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Key binding specific configuration
;;; Code:

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

;; Remap dynamic-abbrev to hippie-expand
;; See https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; TODO add general configuration here
(use-package general
  :config
  (dolist (key '("C-M-<SPC>" "M-<SPC>"))
    (global-unset-key (kbd key)))
  (general-create-definer general-leader :prefix "M-<SPC>")
  (general-def "C-M-<SPC>" 'cycle-spacing)

  (general-leader
   "f"   '(:ignore t :wk "File")
   "ff"  #'(project-find-file :wk "Find in Project")
   ))

;; (use-package general
;;   :config
;;   (general-define-key
;;    :states '(normal visual insert emacs)
;;    :prefix "SPC"
;;    :non-normal-prefix "M-SPC"
;;    "SPC" '(counsel-M-x :which-key "M-x")
;;    "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
;;    "b" '(:ignore t :which-key "buffer")
;;    "bb" '(counsel-switch-buffer :which-key "switch buffer")
;;    "bd" '(kill-current-buffer :which-key "kill buffer")
;;    "bn" '(next-buffer :which-key "next buffer")
;;    "bp" '(previous-buffer :which-key "previous buffer")
;;    "e" '(:ignore t :which-key "edit")
;;    "ee" '(eval-last-sexp :which-key "eval last sexp")
;;    "f" '(:ignore t :which-key "file")
;;    "ff" '(counsel-find-file :which-key "find file")
;;    "fs" '(save-buffer :which-key "save file")
;;    "g" '(:ignore t :which-key "git")
;;    "gs" '(magit-status :which-key "magit status")
;;    "h" '(:ignore t :which-key "help")
;;    "hf" '(describe-function :which-key "describe function")
;;    "hk" '(describe-key :which-key "describe key")
;;    "hv" '(describe-variable :which-key "describe variable")
;;    "p" '(:ignore t :which-key "project")
;;    "pf" '(counsel-projectile-find-file :which-key "find file")
;;    "pp" '(counsel-projectile-switch-project :which-key "switch project")
;;    "q" '(:ignore t :which-key "quit")
;;    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
;;    "s" '(:ignore t :which-key "search")
;;    "ss" '(swiper :which-key "swiper")
;;    "w" '(:ignore t :which-key "window")
;;    "wd" '(delete-window :which-key "delete window")
;;    "wh" '(windmove-left :which-key "move left")
;;    "wj" '(windmove-down :which-key "move down")
;;    "wk" '(windmove-up :which-key "move up")
;;    "wl" '(windmove-right :which-key )))

;; 
(provide 'config-keybindings)
;;; config-keybindings.el ends here
