(use-package exec-path-from-shell       ; Set up environment variables
  :if (display-graphic-p)
  :unless (or (eq system-type 'windows-nt) (eq system-type 'gnu/linux))
  :config
  (setq exec-path-from-shell-variables
        '("PATH"               ; Full path
          "INFOPATH"           ; Info directories
          "GOPATH"             ; Golang path
          ))
  (exec-path-from-shell-initialize))

(setenv "PAGER" "cat")
(setenv "TERM" "xterm-256color")
