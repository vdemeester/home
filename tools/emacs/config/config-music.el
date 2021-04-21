;;; config-music.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Music configuration
;;; Code:

(use-package bongo
  :commands (bongo bongo-show)
  :config
  (setq-default bongo-audio-file-name-extensions '("669" "aac" "amf" "apun" "au" "dsm" "far" "flac" "g18" "g36" "gdm" "imf" "it" "mdz" "med" "mid" "midi" "mka" "mod" "m4a" "mp2" "mp3" "mtm" "ogg" "okt" "r36" "ra" "rcp" "rmi" "s3m" "spx" "stm" "stx" "ult" "umx" "uni" "vqf" "wav" "wma" "xm")))

(provide 'config-music)
;;; config-music ends here
