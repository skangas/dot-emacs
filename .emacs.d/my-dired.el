
;; (setq dired-guess-shell-alist-user
;;       (list
;;        (list (concat my-video-types "\\.$" "FOO-COMMAND");; fixed rule
;;              ;; possibly more rules...
;;              (list "\\.bar$";; rule with condition test
;;                    '(if CONDITION
;;                         "BAR-COMMAND-1"
;;                       "BAR-COMMAND-2"))))

(setq dired-listing-switches "-alh")

;; Add media files to be played by mplayer
(when (boundp 'my-video-types)
  (add-to-list 'dired-guess-shell-alist-user
                `(,(concat my-video-types "$") "mplayer -idx")))


;; Search filenames only
(add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map (kbd "C-s") 'dired-isearch-filenames)))

;; Load Dired-x when Dired is loaded to enable some extra commands.
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))

;; Sanitize n and p
(defun dired-next-file-line ()
  "Moves to the next dired line that have a file or directory name on it"
  (interactive)
  (call-interactively 'dired-next-line)
  (if (not (dired-move-to-filename))
      (dired-next-file-line)))
(defun dired-previous-file-line ()
  "Moves to the previous dired line that have a file or directory name on it"
   (interactive)
   (call-interactively 'dired-previous-line)
   (if (not (dired-move-to-filename))
       (dired-previous-file-line)))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map "n" 'dired-next-file-line)))
(add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map "p" 'dired-previous-file-line)))

;; TODO: WRITE command for switching between showing dot-files and not

;; we want dired to use only one buffer for all directories
;; (eval-after-load "dired"
;;   '(progn
;;      (defadvice dired-advertised-find-file (around dired-subst-directory activate)
;;        "Replace current buffer if file is a directory."
;;        (interactive)
;;        (let* ((orig (current-buffer))
;; 	      (filename (dired-get-filename))
;; 	      (bye-p (file-directory-p filename)))
;; 	 ad-do-it
;; 	 (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
;; 	   (kill-buffer orig))))))

; jump to a file by typing that filename's first character -- DISABLED FOR NOW
;(require 'dired-view)
;(add-hook 'dired-mode-hook 'dired-view-minor-mode-on) ; enable by default
;; (add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map (kbd ";") 'dired-view-minor-mode-toggle)))
;; (add-hook 'dired-mode-hook '(lambda () (define-key dired-mode-map (kbd ":") 'dired-view-minor-mode-dired-toggle)))

;; Enable toggling of uninteresting files.
(setq dired-omit-files-p t)
(setq dired-omit-files "^\\.?#\\|^\\.svn$\\|^\\.$")

(provide 'my-dired)

;; my-dired.el ends here
