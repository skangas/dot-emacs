;;; init-keybindings.el


;;; Settings

(when (display-graphic-p)
  ;; Disable keyboard iconfying
  (bind-key "C-z" 'isearch-forward)
  ;; (unbind-key "C-z")
  )

;; Unset some useless keybindings
(unbind-key "C-x C-z") ; suspend-frame


;;; Advice

(defun sk/advice-recenter-top (orig-fun &rest args)
  (apply orig-fun args)
  (recenter-top-bottom 0))

(add-hook 'next-error-hook 'recenter)


;;; Global key bindings

;; C-<foo>
(bind-key "C-k" #'my/kill-and-join-forward)
(bind-key "C-=" #'text-scale-increase)
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease) ; use ´C-u -' for negative prefix

(bind-key "C-c 1" 'sk/org-agenda)
(bind-key "C-c 2" 'sk/notmuch-inbox)
(bind-key "C-c 3" 'elfeed)
(bind-key "C-c 4" 'notmuch)
(bind-key "C-c 5" 'magit-status)

;; (bind-key "-/" 'hippie-expand) ; Remove?

;; (define-prefix-command 'ctl-ao-map)
;; (bind-key "C-ä" 'ctl-ao-map)
;; (bind-key "C-ä C-ä" #'switch-bury-or-kill-buffer)
;; (bind-key "C-ä C-c" #'compile)
;; (bind-key "C-ä C-b" #'previous-buffer)
;; (bind-key "C-ä C-f" #'next-buffer)
;; (bind-key "C-ä C-p" #'winner-undo)
;; (bind-key "C-ä C-n" #'winner-redo)

;; M-<foo>
(bind-key "M-<left>" 'previous-buffer)
(bind-key "M-<right>" 'next-buffer)
(bind-key "M-/" #'hippie-expand)
(bind-key "M-z" 'zap-up-to-char)

(bind-key "M-c" #'capitalize-dwim)
(bind-key "M-u" #'upcase-dwim)
(bind-key "M-l" #'downcase-dwim)
(unbind-key "C-x C-l") ; default is `downcase-region'
(unbind-key "C-x C-u") ; default is `upcase-region'

;; F<foo>
(bind-key "<f5>" #'my-switch-to-gnus)
(bind-key "<f6>" #'mentor)
(bind-key "<f8>" #'w3m)

;; C-x <foo>
(bind-key "C-x M"   #'compose-mail)
(bind-key "C-x m"   #'browse-url-at-point)
(bind-key "C-x j"   #'duplicate-dwim)
(bind-key "C-x x e" (if (fboundp 'elide-head-mode) ; Emacs 29
                        #'elide-head-mode
                      'elide-head))
(bind-key "C-x x q" #'read-only-mode)
(bind-key "C-x C-b" #'ibuffer)
(bind-key "C-x C-r" #'recentf-open) ; replaces `find-file-read-only'

;; C-c <foo>
(bind-key "C-c a" #'org-agenda)
(bind-key "C-c b" #'org-iswitchb)
(bind-key "C-c B" #'gnus-read-ephemeral-emacs-bug-group)
(bind-key "C-c c" #'org-capture)
(bind-key "C-c l" #'org-store-link)
(bind-key "C-c q" #'refill-mode)
(bind-key "C-c y" (lambda () (interactive) (popup-menu 'yank-menu)))
(bind-key "C-c C-c M-x" 'execute-extended-command) ;; Remove?

;; C-c e <foo>
(bind-key "C-c e F" #'flyspell-mode)
(bind-key "C-c e a" #'aggressive-indent-mode)
(bind-key "C-c e d" #'toggle-debug-on-error)
(bind-key "C-c e i" #'my-eval-and-replace)
(bind-key "C-c e m" #'macrostep-expand)
(bind-key "C-c e r" #'eval-region)
(bind-key "C-c e s" #'scratch-buffer)
;; (bind-key "C-c e b" #'eval-buffer) ; moved to emacs-lisp-mode
;; (bind-key "C-c e f" #'emacs-lisp-byte-compile-and-load) ; moved to emacs-lisp-mode
;; (bind-key "C-c e n" #'nameless-mode) ; moved to emacs-lisp-mode
;; (bind-key "C-c e t" #'sk/ert-run-all-tests) ; moved to emacs-lisp-mode

;; C-g
(bind-key "M-g M-r" #'goto-random-line)
(bind-key "M-g M-l" #'list-packages)
(bind-key "M-g M-m" #'my-menu-bar-mode)
(bind-key "M-g M-s" #'sort-lines)
(bind-key "M-g M-w" #'eww)

;; C-h
(bind-keys :map help-map
           ("u" . man )
           ("C-b" . which-key-show-major-mode)
           ("C-a" . apropos))


;;; My utility functions

;; This is no longer needed; I use Hunspell instead.
;; (defun sk/use-swedish-dictionary ()
;;   (interactive)
;;   (ispell-change-dictionary "swedish")
;;   (flyspell-buffer))

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

(bind-key "<next>" #'my-scroll-up)
(bind-key "<prior>" #'my-scroll-down)

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


;;;; Various jump commands

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

;; From:
;; https://writequit.org/org/#6a7b04f8-ab04-4d6f-a3ef-3a907aa0e5d2
(defun my/kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(provide 'init-keybindings)
