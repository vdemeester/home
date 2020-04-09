;;; 01-server.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Enable server-mode
;;; Code:

;; UseServer
(use-package server
  :config (or (server-running-p) (server-mode)))
;; -UseServer

;;; 01-server.el ends here
