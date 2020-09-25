;;; config-keybindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Key binding specific configuration
;;; Code:

;; Disable C-x C-n to avoid the disabled command buffer
(unbind-key "C-x C-n" global-map)

(provide 'config-keybindings)
;;; config-keybindings.el ends here
