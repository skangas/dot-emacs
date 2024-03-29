;; (use-package emms
;;   :config
;;   (require 'emms-setup)
;;   (emms-all)
;;   (setq emms-player-list
;;         emms-setup-default-player-list)
;;   (emms-default-players)

;;   (setq emms-source-file-default-directory "~/music/")
;;   (setq emms-player-mpd-music-directory "~/music/")
;;   )

;;;;;;; OLD STUFF -- REMOVE?

;; (require 'emms-setup)
;; (emms-all)
;; (emms-default-players)

;; (require 'emms-player-mpd)
;; (setq emms-player-mpd-server-name "localhost")
;; (setq emms-player-mpd-server-port "8000")

;; (add-to-list 'emms-info-functions 'emms-info-mpd)
;; (add-to-list 'emms-player-list 'emms-player-mpd)

;; (setq emms-playlist-buffer-name "*EMMS Playlist*")
;; (setq later-do-interval 0.5)

;; emms-source-file-directory-tree-function
;; (setq emms-mode-line-format " %s "
;;       emms-show-format "NP: %s"
;;       emms-player-mpd-server-name "localhost"
;;       emms-player-mpd-server-port "6600")

;; (bind-key "<XF86AudioPlay>" 'emms-pause)
;; (bind-key "<XF86AudioStop>" 'emms-stop)
;; (bind-key "<XF86AudioPrev>" 'emms-previous)
;; (bind-key "<XF86AudioNext>" 'emms-next)
;; (bind-key "<XF86>" 'de-add-dir)
;; (bind-key "<XF86>" 'emms-smart-browse)
;; (bind-key "<XF86ight>" 'emms-seek-forward)
;; (bind-key "<XF86eft>" 'emms-seek-backward)

;; caching stuff
;; (emms-cache 1)
;; (setq later-do-interval 0.0001
;;       emms-info-asynchronously t)

(provide 'init-emms)
