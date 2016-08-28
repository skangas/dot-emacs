(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/emms/lisp"))
(require 'emms-auto)
(autoload 'emms-browser "emms-setup" "emms-setup" t)

(eval-after-load "emms"
  '(progn
     (require 'emms-setup)
     (emms-devel)
     (emms-default-players)

     (require 'emms-player-mpd)
     (add-to-list 'emms-player-list 'emms-player-mpd)
     (add-to-list 'emms-info-functions 'emms-info-mpd)
     
     (setq emms-playlist-buffer-name "*EMMS Playlist*"
           emms-source-file-default-directory "~/music/"
           ;; emms-source-file-directory-tree-function 
           emms-player-mpd-music-directory "~/music/"
           emms-mode-line-format " %s "
           emms-show-format "NP: %s"
           emms-player-mpd-server-name "localhost"
           emms-player-mpd-server-port "6600")

     ;; (global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
     ;; (global-set-key (kbd "<XF86AudioStop>") 'emms-stop)
     ;; (global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
     ;; (global-set-key (kbd "<XF86AudioNext>") 'emms-next)
                                        ;(global-set-key (kbd "<XF86>") 'de-add-dir)
                                        ;(global-set-key (kbd "<XF86)" 'emms-smart-browse)
                                        ;(global-set-key (kbd "<XF86ight>") 'emms-seek-forward)
                                        ;(global-set-key (kbd "<XF86eft>") 'emms-seek-backward)

     ;; caching stuff
     (emms-cache 1)
     (setq later-do-interval 0.0001
           emms-info-asynchronously t)))

(provide 'my-emms)

;; my-emms.el ends here