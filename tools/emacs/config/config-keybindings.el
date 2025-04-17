;;; config-keybindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Key binding specific configuration
;;; Code:

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

;; Remap dynamic-abbrev to hippie-expand
;; See https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package vde-simple
  :demand t
  :preface
  (dolist (key '("C-M-<SPC>" "M-<SPC>"))
    (global-unset-key (kbd key)))
  :bind
  (("M-<SPC> e" . dired-jump)
   ("M-<SPC> x" . execute-extended-command)
   ("M-<SPC> :" . eval-expression)
   ("M-<SPC> <SPC>" . (lambda()(interactive)
			(let ((consult-buffer-filter))
			  (add-to-list 'consult-buffer-filter "\\*")
			  (call-interactively 'consult-buffer))))
   ("M-<SPC> f f" . project-find-file)
   ("M-<SPC> f o" . ffap)
   ("M-<SPC> f q" . read-only-mode)
   ("M-<SPC> f r" . consult-recent-file)
   ("M-<SPC> q q" . save-buffers-kill-terminal)))

;; TODO add general configuration here
;; (use-package general
;;   :config
;;   (dolist (key '("C-M-<SPC>" "M-<SPC>"))
;;     (global-unset-key (kbd key)))
;;   (general-create-definer general-leader :prefix "M-<SPC>")
;;   (general-def "C-M-<SPC>" 'cycle-spacing)
;; 
;;   (general-leader
;;     "z"   #'(repeat :which-key "Repeat")
;;     "u"   #'(universal-argument :which-key "Universal argument")
;;     "e"   #'(dired-jump :which-key "Dired")
;;     "x"   #'(execute-extended-command :which-key "M-x")
;;     "f"   '(:ignore t :which-key "File")
;;     "SPC" #'((lambda()(interactive)
;;                (let ((consult-buffer-filter))
;;                  (add-to-list 'consult-buffer-filter "\\*")
;;                  (call-interactively 'consult-buffer))) :wk "Switch to Buffer")
;;     ":"   #'(eval-expression :wk "Eval expression")
;;     "ff"  #'(project-find-file :which-key "Find in Project")
;;     "fo"  #'(ffap :which-key "Find with context")
;;     "fq"  #'(read-only-mode :which-key "Toggle Read Only")
;;     "fr"  #'(consult-recent-file :which-key "Recent File")
;;     "q"   #'(:ignore t :wk "Quit")
;;     "qq"  #'(save-buffers-kill-terminal :wk "Quit Emacs")
;;    ))

;; 
(provide 'config-keybindings)
;;; config-keybindings.el ends here
