;;; my-keybindings.el

(setq cua-enable-cua-keys nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

;; FIXME: Make this into a toggle...
(defun my-use-swedish-dictionary ()
  (interactive)
  (ispell-change-dictionary "swedish")
  (flyspell-buffer))

(defun my-translate-using-tyda (&optional arg)
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

(defun my-emms-browser ()
  "Jump to EMMS"
  (interactive)
  ;; (emms-cache-set-from-mpd-all)
  (select-frame (make-frame '((name . "EMMS"))))
  (emms-player-mpd-connect)
  (emms-browser))

;; open recent files using ido
(defun my-ido-recentf-open ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; occur-mode
(defun my-occur-mode-keybindings ()
  (define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)
  (define-key occur-mode-map (kbd "n") 'next-logical-line)
  (define-key occur-mode-map (kbd "p") 'previous-logical-line))
(add-hook 'occur-mode-hook 'my-occur-mode-keybindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page down/up move the point, not the screen.;; (from snarfed.org) This
;; means that pgup/pgdn can move the point to the beginning or end of the
;; buffer.
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

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Switch C-h and C-s for ergonomic reasons
(global-set-key (kbd "C-h") 'isearch-forward)
(global-set-key (kbd "C-s") 'help-command)
(define-key isearch-mode-map "\C-h" 'isearch-repeat-forward)
(define-key isearch-mode-map "\C-s" 'help-command)
(define-key ido-common-completion-map "\C-h" 'ido-next-match)
(define-key ido-common-completion-map "\C-s" 'help-command)

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
;; (global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e i") 'my-eval-and-replace)
(global-set-key (kbd "C-c e r") 'eval-region)
;; (global-set-key (kbd "C-c e s") 'scratch)
(global-set-key (kbd "C-c e m") 'macrostep-expand)

(global-unset-key (kbd "C-s e"))
(global-set-key (kbd "C-s e e") 'view-echo-area-messages)
(global-set-key (kbd "C-s e f") 'find-function)
(global-set-key (kbd "C-s e k") 'find-function-on-key)
(global-set-key (kbd "C-s e l") 'find-library)
(global-set-key (kbd "C-s e v") 'find-variable)
(global-set-key (kbd "C-s e V") 'apropos-value)

(global-set-key (kbd "C-<f3>") 'w3m-goto-url-new-session)
(global-set-key (kbd "<f5>") 'my-switch-to-gnus)
(global-set-key (kbd "<f6>") 'mentor)
;; (global-set-key (kbd "<f9> ") 'my-emms-browser)
(global-set-key (kbd "<f8>") 'w3m)
(global-set-key (kbd "<f11>") 'compile)
(global-set-key (kbd "<C-tab>") 'hippie-expand)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-b") 'org-iswitchb)
(global-set-key (kbd "C-c S") 'my-use-swedish-dictionary)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'bbdb)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c t") 'my-translate-using-tyda)
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)
;; (global-set-key (kbd "C-x C-b") 'bs-show)
;; (global-set-key (kbd "C-x f") 'djcb-find-file-as-root)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x m") 'browse-url-at-point)
;; OK, Steve Yegge, I'll give it a try
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-r") 'my-ido-recentf-open) ;; replace `find-file-read-only'

(when window-system (global-unset-key "\C-z"))       ; Disable keyboard iconfying

(provide 'my-keybindings)

;; my-keybindings.el ends here