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

;; (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
;; (global-set-key (kbd "<XF86AudioStop>") 'emms-stop)
;; (global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
;; (global-set-key (kbd "<XF86AudioNext>") 'emms-next)
;;(global-set-key (kbd "<XF86>") 'de-add-dir)
;;(global-set-key (kbd "<XF86)" 'emms-smart-browse)
;;(global-set-key (kbd "<XF86ight>") 'emms-seek-forward)
;;(global-set-key (kbd "<XF86eft>") 'emms-seek-backward)

;; caching stuff
;; (emms-cache 1)
;; (setq later-do-interval 0.0001
;;       emms-info-asynchronously t)

(provide 'init-emms)
