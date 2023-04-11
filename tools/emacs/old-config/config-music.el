;;; config-music.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Music configuration
;;; Code:

;; (use-package bongo
;;   :commands (bongo bongo-show)
;;   :bind (("C-c x b" . bongo)
;;          ("<C-XF86AudioPlay>" . bongo-pause/resume)
;;          ("<C-XF86AudioNext>" . bongo-next)
;;          ("<C-XF86AudioPrev>" . bongo-previous)
;;          ("<M-XF86AudioPlay>" . bongo-show)
;;          ("<S-XF86AudioNext>" . bongo-seek-forward-10)
;;          ("<S-XF86AudioPrev>" . bongo-seek-backward-10))
;;   :config
;;   (setq-default bongo-audio-file-name-extensions
;;                 '("669" "aac" "amf" "apun" "au" "dsm" "far" "flac" "g18" "g36" "gdm"
;;                   "imf" "it" "mdz" "med" "mid" "midi" "mka" "mod" "m4a" "mp2" "mp3"
;;                   "mtm" "ogg" "oga ""opus" "okt" "r36" "ra" "rcp" "rmi" "s3m" "spx"
;;                   "stm" "stx" "ult" "umx" "uni" "vqf" "wav" "wma" "xm")
;;                 bongo-custom-backend-matchers
;;                 '((mpv local-file "m4a")
;;                   (mpv local-file "oga")
;;                   (mpv local-file "opus"))
;;                 bongo-default-directory "/net/sakhalin.home/export/gaia/music"
;;                 bongo-prefer-library-buffers nil
;;                 bongo-insert-whole-directory-trees t
;;                 bongo-logo nil
;;                 bongo-display-track-icons nil
;;                 bongo-display-track-lengths nil
;;                 bongo-display-header-icons nil
;;                 bongo-display-playback-mode-indicator t
;;                 bongo-display-inline-playback-progress nil
;;                 bongo-join-inserted-tracks nil
;;                 bongo-field-separator (propertize " · " 'face 'shadow))
;;   (bongo-mode-line-indicator-mode -1)
;;   (bongo-header-line-mode -1))

(provide 'config-music)
;;; config-music ends here
