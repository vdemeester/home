;;; programming-treesitter.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Treesitter configuration
;;; Code:


(use-package indent-bars
  :if (eq system-type 'gnu/linux)
  :hook
  (python-mode . indent-bars-mode)
  (yaml-ts-mode . indent-bars-mode)
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-no-descend-lists t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                               if_statement with_statement while_statement))))

(provide 'programming-treesitter)
;;; programming-treesitter.el ends here
