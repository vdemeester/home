;;; programming-treesitter.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Treesitter configuration
;;; Code:

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package treesit
  :config
  (use-package combobulate
    :preface
    (unless (package-installed-p 'combobulate)
      (package-vc-install "https://github.com/mickeynp/combobulate"))
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook
      ((python-ts-mode . combobulate-mode)
       (js-ts-mode . combobulate-mode)
       (html-ts-mode . combobulate-mode)
       (css-ts-mode . combobulate-mode)
       (yaml-ts-mode . combobulate-mode)
       (typescript-ts-mode . combobulate-mode)
       (json-ts-mode . combobulate-mode)
       (tsx-ts-mode . combobulate-mode)
       (go-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("path-to-git-checkout-of-combobulate")))

(provide 'programming-treesitter)
;;; programming-treesitter.el ends here
