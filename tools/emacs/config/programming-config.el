;;; programming-config.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration files mode configuration
;;; Code:

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-current-column-mode)))

(use-package conf-mode
  :mode ("\\.to?ml\\'" . conf-toml-mode))

(provide 'programming-config)
;;; programming-config.el ends here
