;;; init-keybindings.el


;;; Settings

(setq cua-enable-cua-keys nil) ; Disable cua keys
(when window-system (global-unset-key "\C-z")) ; Disable keyboard iconfying


;;; Advice

(defun sk/advice-recenter-top (orig-fun &rest args)
  (apply orig-fun args)
  (recenter-top-bottom 0))

(add-hook 'next-error-hook 'recenter)


;;; Global key bindings

(defun sk/notmuch-inbox (arg)
  "Show notmuch inbox, with prefix arg show notmuch."
  (interactive "P")
  (require 'notmuch)
  (if arg
      (notmuch)
    (notmuch-search "tag:inbox")))

(defun sk/org-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda(x)*")
      (switch-to-buffer "*Org Agenda(x)*")
    (org-agenda nil "x")))

(defun sk/mailsync.sh ()
  (interactive)
  (async-shell-command "mailsync.sh"))

;; C-<foo>
(dolist (k '("C-" ""))
  (global-set-key (kbd (concat "C-c " k "1")) 'sk/org-agenda)
  (global-set-key (kbd (concat "C-c " k "2")) 'sk/notmuch-inbox)
  (global-set-key (kbd (concat "C-c " k "3")) 'elfeed)
  (global-set-key (kbd (concat "C-c " k "4")) 'notmuch)
  (global-set-key (kbd (concat "C-c " k "5")) 'magit-status))

(global-set-key (kbd "C-z") 'isearch-forward)
(global-set-key (kbd "C-M-y") 'iedit-mode)
(global-set-key (kbd "C-!") 'org-capture)
;; (global-set-key (kbd "-/") 'hippie-expand) ; Remove?

;; (define-prefix-command 'ctl-ao-map)
;; (global-set-key (kbd "C-ä") 'ctl-ao-map)
;; (global-set-key (kbd "C-ä C-ä") 'switch-bury-or-kill-buffer)
;; (global-set-key (kbd "C-ä C-c") 'compile)
;; (global-set-key (kbd "C-ä C-b") 'previous-buffer)
;; (global-set-key (kbd "C-ä C-f") 'next-buffer)
;; (global-set-key (kbd "C-ä C-p") 'winner-undo)
;; (global-set-key (kbd "C-ä C-n") 'winner-redo)

;; M-<foo>
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; F<foo>
(global-set-key (kbd "<f5>") 'my-switch-to-gnus)
(global-set-key (kbd "<f6>") 'mentor)
(global-set-key (kbd "<f8>") 'w3m)

;; C-x <foo>
(global-set-key (kbd "C-x m") 'browse-url-at-point)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") #'recentf-open) ; replaces `find-file-read-only'

;; C-c <foo>
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c B") 'gnus-read-ephemeral-emacs-bug-group)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c t") 'sk/translate-using-tyda)
(global-set-key (kbd "C-c y") (lambda () (interactive) (popup-menu 'yank-menu)))
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; Remove?

;; C-c e <foo>
(global-set-key (kbd "C-c e a") 'aggressive-indent-mode)
(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e d") 'toggle-debug-on-error)
(global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e i") 'my-eval-and-replace)
(global-set-key (kbd "C-c e n") 'nameless-mode)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e s") 'scratch)
(global-set-key (kbd "C-c e m") 'macrostep-expand)
(global-set-key (kbd "C-c e t") 'sk/ert-run-all-tests)

;; C-g
(global-set-key (kbd "M-g M-r") #'goto-random-line)
(global-set-key (kbd "M-g M-m") #'my-menu-bar-mode)
(global-set-key (kbd "M-g M-s") #'sort-lines)

(global-set-key (kbd "C-x x e") (if (fboundp 'elide-head-mode) ; Emacs 29
                                    #'elide-head-mode
                                  #'elide-head))

(define-key ctl-x-map "\C-j" 'dired-jump)

;; C-h
(define-key help-map "u" 'man)
(define-key help-map "\C-a" 'apropos)

;; (global-set-key "\C-t" 'shell-pop)
;; (global-set-key "\C-c\C-k" 'kill-region)


;;; Mode dependent key bindings

(customize-set-variable 'smerge-command-prefix (kbd "C-c v"))

;; occur-mode
(define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-logical-line)
(define-key occur-mode-map (kbd "p") 'previous-logical-line)

;; wgrep
(with-eval-after-load 'grep
  (define-key grep-mode-map
              (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(with-eval-after-load 'wgrep
  (define-key grep-mode-map
              (kbd "C-c C-c") 'wgrep-finish-edit))


;;; My utility functions

(defun sk/use-swedish-dictionary ()
  ;; This is no longer needed; I use Hunspell instead.
  (interactive)
  (ispell-change-dictionary "swedish")
  (flyspell-buffer))

(defun sk/translate-using-tyda (&optional arg)
  "Translate word at point using tyda.nu"
  (interactive "P")
  (let* ((ord (thing-at-point 'word))
         (url (concat "http://tyda.se/search?form=1&w=" ord)))
    (if ord
        (browse-url-default-browser url
                                    (if arg
                                        (not browse-url-new-window-flag)
                                      browse-url-new-window-flag))
      (error "No word at point"))))


;;; Various hacks

(defun switch-bury-or-kill-buffer (&optional aggr)
  "With no argument, switch (but unlike C-x b, without the need
to confirm).  With C-u, bury current buffer.  With double C-u,
kill it (unless it's modified)."
  (interactive "P")
  (cond
   ((eq aggr nil) (switch-to-buffer (other-buffer)))
   ((equal aggr '(4)) (bury-buffer))
   ((equal aggr '(16)) (kill-buffer-if-not-modified (current-buffer)))))

;; Page down/up move the point, not the screen. (from snarfed.org) This means
;; that pgup/pgdn can move the point to the beginning or end of the buffer.
(defun my-scroll-down ()
  (interactive)
  (condition-case nil (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(defun my-scroll-up ()
  (interactive)
  (condition-case nil (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(global-set-key [next] 'my-scroll-up)
(global-set-key [prior] 'my-scroll-down)

;; copy current line without selecting it (courtesy of emacs-fu)
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end))
                 (message "Copied line")
                 (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defun my-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case err
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (insert (current-kill 0))
           (error err))))

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))


;;; Currently unused

;; (defun sk/emms-browser ()
;;   "Jump to EMMS."
;;   (interactive)
;;   ;; (emms-cache-set-from-mpd-all)
;;   (select-frame (make-frame '((name . "EMMS"))))
;;   (emms-player-mpd-connect)
;;   (emms-browser))

;; (defun my-switch-to-gnus (&optional arg)
;;   (interactive)
;;   "Switch to a Gnus related buffer, or start gnus if it's not running.
;;     Candidates are buffers starting with
;;      *mail or *reply or *wide reply
;;      *Summary or
;;      *Group*"
;;   (let (candidate
;;         (alist '(("^\\*\\(mail\\|\\(wide \\)?reply\\)" t)
;;                  ("^\\*Group")
;;                  ("^\\*Summary")
;;                  ("^\\*Article" nil (lambda ()
;;                                       (buffer-live-p gnus-article-current-summary))))))
;;     (catch 'none-found
;;       (dolist (item alist)
;;         (let (last
;;               (regexp (nth 0 item))
;;               (optional (nth 1 item))
;;               (test (nth 2 item)))
;;           (dolist (buf (buffer-list))
;;             (when (and (string-match regexp (buffer-name buf))
;;                        (> (buffer-size buf) 0))
;;               (setq last buf)))
;;           (cond ((and last (or (not test) (funcall test)))
;;                  (setq candidate last))
;;                 (optional
;;                  nil)
;;                 (t
;;                  (throw 'none-found t))))))
;;     (cond ((or (not (fboundp 'gnus-alive-p))
;;                (not (gnus-alive-p)))
;;            (gnus))
;;           (candidate
;;            (switch-to-buffer candidate)))))


;;; Temporarily enable menu-bar

(defvar my-menu-bar-timer nil)

(defun my-menu-bar-cancel-timer ()
  (when my-menu-bar-timer
    (cancel-timer my-menu-bar-timer)
    (setq my-menu-bar-timer nil)))

(defun my-menu-bar-mode-disable ()
  (my-menu-bar-cancel-timer)
  (menu-bar-mode -1))

(defun my-menu-bar-mode ()
  (interactive)
  (my-menu-bar-cancel-timer)
  (menu-bar-mode 1)
  (run-with-idle-timer 60 nil #'my-menu-bar-mode-disable))

(provide 'init-keybindings)
