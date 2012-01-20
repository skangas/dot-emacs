;; Load Dired-x when Dired is loaded to enable some extra commands.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;;; Use human sizes
(setq dired-listing-switches "-lAh")

;; Search filenames only
(setq dired-isearch-filenames 'dwim)

;; openwith.el -- open files using external helpers

(require 'openwith)
(openwith-mode t)

(setq my-video-types '(".asf" ".avi" ".f4v"
                       ".flv" ".m4a" ".mkv"
                       ".mov" ".mp4" ".mpeg"
                       ".mpg" ".ogv" ".wmv"))
(setq my-video-types-regexp (regexp-opt my-video-types))

(setq openwith-associations
      (let ((video-types (concat my-video-types-regexp "\\'")))
        `((,video-types "mplayer" ("-idx" file))
          ("\\.img\\'" "mplayer" ("dvd://" "-dvd-device" file))
          ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
          ;; ("\\.mp3\\'" "mplayer" (file))
          ;; ("\\.pdf\\'" "evince" (file))
          )))

;; Toggle showing dot-files using "."

(define-minor-mode dired-hide-dotfiles-mode
  ""
  :lighter " Hide"
  :init-value nil
  (if (not (eq major-mode 'dired-mode))
      (progn 
	(error "Doesn't seem to be a Dired buffer")
	(setq dired-hide-dotfiles-mode nil))
    (if dired-hide-dotfiles-mode
	(setq dired-actual-switches "-lh")
      (setq dired-actual-switches "-lAh"))
    (revert-buffer)))

(define-key dired-mode-map "." 'dired-hide-dotfiles-mode)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-dotfiles-mode 1)))

(provide 'my-dired)

;; my-dired.el ends here
