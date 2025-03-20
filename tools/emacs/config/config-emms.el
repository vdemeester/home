;;; config-emms.el --- EMMS configuration to play music from Emacs -*- lexical-binding: t -*-

;; Author: Vincent Demeester

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This configuration is used for controlling playing music from Emacs.

;;; Code:

(use-package emms
  :custom
  (emms-player-list '(emms-player-mpv))
  (emms-source-file-default-directory "~/desktop/music")
  (emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=null"))
  :config
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-player-simple)
  (require 'emms-player-mpv)
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-info-mp3info)
  (require 'emms-info-ogginfo)
  (require 'emms-info-opusinfo)
  (require 'emms-info-metaflac)
  (require 'emms-info-tinytag)
  (require 'emms-info-exiftool)
  (require 'emms-info-native)
  (require 'emms-cache)
  (require 'emms-mode-line)
  (require 'emms-mark)
  (require 'emms-show-all)
  (require 'emms-streams)
  (require 'emms-playing-time)
  (require 'emms-browser)
  (require 'emms-mode-line-icon)
  (require 'emms-cue)
  (require 'emms-bookmarks)
  (require 'emms-last-played)
  (require 'emms-metaplaylist-mode)
  (require 'emms-stream-info)
  (require 'emms-history)
  (require 'emms-i18n)
  (require 'emms-volume)
  (require 'emms-playlist-limit)
  (require 'emms-mpris)
  (require 'emms-idapi-musicbrainz)
  (require 'emms-idapi-browser)
  
  (setq emms-playlist-default-major-mode #'emms-playlist-mode)
  (add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
  (setq emms-info-functions '(emms-info-native emms-info-cueinfo))
  (setq emms-track-description-function #'emms-info-track-description)
  (when (fboundp 'emms-cache)		; work around compiler warning
    (emms-cache 1))
  (emms-mode-line-mode 1)
  (emms-mode-line-blank)
  (emms-playing-time-mode 1)
  (add-hook 'emms-player-started-hook #'emms-last-played-update-current))

(provide 'config-emms)
;;; config-emms.el ends here
