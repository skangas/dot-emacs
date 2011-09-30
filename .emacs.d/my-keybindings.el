;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings

;; TODO: autoload
(require 'boxquote)

(global-set-key (kbd "<C-tab>") 'hippie-expand)
;; (global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'bbdb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x m") 'browse-url-at-point)
(global-set-key (kbd "C-c S")    
  (lambda()(interactive)
    (ispell-change-dictionary "swedish")
    (flyspell-buffer)))
(global-set-key (kbd "C-c s")
                (lambda ()
                  (interactive)
                  (sr-speedbar-toggle)
                  (sr-speedbar-select-window)))

(global-set-key (kbd "M-g") 'goto-line)

;; OK, Steve Yegge, I'll give it a try
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function keys
(global-set-key [(control f3)] 'w3m-goto-url-new-session)
(global-set-key [f5] 'my-switch-to-gnus)
(global-set-key [(control f5)]
                '(lambda ()
                   (interactive)
                   (gnus-group-get-new-news)))
;; (global-set-key [(control shift f5)]
;;                 '(lambda ()
;;                    (interactive)
;;                    (let ((buffer (get-buffer "*nnmail split history*")))
;;                      (delete-windows-on buffer)
;;                      (bury-buffer buffer))))

(defun my-emms-browser ()
  (interactive)
  ;; (emms-cache-set-from-mpd-all)
  (select-frame (make-frame '((name . "EMMS"))))
  (emms-player-mpd-connect)
  (emms-browser))

(global-set-key [f6] 'my-emms-browser)
(global-set-key [f7] 'w3m)
(global-set-key [f9] 'org-remember)
(global-set-key [(meta f11)] 'my-ido-choose-from-recentf)
(global-set-key [f12] 'compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; resize window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy current line without selecting it (courtesy of emacs-fu)

(defadvice kill-ring-save (before slick-copy activate compile) "When called
interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position) (line-beginning-position 2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page down/up move the point, not the screen.;; (from snarfed.org) This
;; means that pgup/pgdn can move the point to the beginning or end of the
;; buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))
(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-switch-to-gnus (&optional arg)
  (interactive)
  "Switch to a Gnus related buffer, or start gnus if it's not running.
    Candidates are buffers starting with
     *mail or *reply or *wide reply
     *Summary or
     *Group*"
  (let (candidate
        (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
                 ("^\\*Group")
                 ("^\\*Summary")
                 ("^\\*Article" nil (lambda ()
                                      (buffer-live-p gnus-article-current-summary))))))
    (catch 'none-found
      (dolist (item alist)
        (let (last
              (regexp (nth 0 item))
              (optional (nth 1 item))
              (test (nth 2 item)))
          (dolist (buf (buffer-list))
            (when (and (string-match regexp (buffer-name buf))
                       (> (buffer-size buf) 0))
              (setq last buf)))
          (cond ((and last (or (not test) (funcall test)))
                 (setq candidate last))
                (optional
                 nil)
                (t
                 (throw 'none-found t))))))
    (cond ((or (not (fboundp 'gnus-alive-p))
               (not (gnus-alive-p)))
           (gnus))
          (candidate
           (switch-to-buffer candidate)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  
  (backward-kill-sexp)
  (condition-case err
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (insert (current-kill 0))
           (error err))))

(global-set-key (kbd "C-c e") 'my-eval-and-replace)

;; (defun my-last-used-buffer ()
;;   (interactive)
;;   (switch-to-buffer (other-buffer)))

;; (global-set-key (kbd "C-'") 'my-last-used-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://groups.google.com/group/gnu.emacs.help/msg/a784fbb684a24e17

(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
        (exchange-point-and-mark))
     (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
       (forward-line -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (move-text-internal arg))

(defun move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (move-text-internal (- arg)))

(global-set-key "\M-P" 'move-text-up)
(global-set-key "\M-N" 'move-text-down) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(provide 'my-keybindings)

;; my-keybindings.el ends here
