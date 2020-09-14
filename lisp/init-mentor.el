;;; init-mentor.el  -*- lexical-binding:t -*-

;; (use-package xml-rpc
;;   :ensure t)

(add-to-list 'load-path (expand-file-name "~/wip/mentor"))
;; Use my local version.
(add-to-list 'load-path (expand-file-name "~/wip/xml-rpc-el"))

;; (setq mentor-rtorrent-external-rpc "scgi://127.0.0.1:5000")
(setq mentor-rtorrent-external-rpc "http://localhost/our-RPC2")
;; (setq mentor-rtorrent-external-rpc "scgi:///~/.rtorrent-session/rpc.socket")

(autoload 'mentor "mentor" nil t)

;; (setq mentor-highlight-enable t)
;; (setq mentor-view-columns
;;       '(((mentor-torrent-get-state) -3 "State")
;;         ((mentor-torrent-get-progress) -3 "Cmp")
;;         (name -40 "Name")
;;         ((mentor-torrent-get-speed-up) -6 "Up")
;;         ((mentor-torrent-get-speed-down) -6 "Down")
;;         ((mentor-torrent-get-size) -15 "     Size")
;;         (message -40 "Message")
;;         (directory -100 "Directory")
;;         (tied_to_file -80 "Tied file name")))

(provide 'init-mentor)
