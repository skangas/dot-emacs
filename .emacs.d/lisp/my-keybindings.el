;;; my-keybindings.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq cua-enable-cua-keys nil)

;; Disable keyboard iconfying
(when window-system (global-unset-key "\C-z"))

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

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

;;; Copy line
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

; optional key binding
(global-set-key "\C-c\C-k" 'copy-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-å") 'isearch-forward) ; more ergonomic alias


(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(global-set-key (kbd "C-c e b") 'eval-buffer)
(global-set-key (kbd "C-c e e") 'toggle-debug-on-error)
;; (global-set-key (kbd "C-c e f") 'emacs-lisp-byte-compile-and-load)
(global-set-key (kbd "C-c e i") 'my-eval-and-replace)
(global-set-key (kbd "C-c e r") 'eval-region)
;; (global-set-key (kbd "C-c e s") 'scratch)
(global-set-key (kbd "C-c e m") 'macrostep-expand)

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
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
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
(global-set-key "\C-t" 'shell-pop)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key (kbd "C-x C-r") 'my-ido-recentf-open) ;; replace `find-file-read-only'

(global-set-key (kbd "C-M-y") 'iedit-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; dired
(defun image-dired-here ()
  "Make a preview buffer for all images in current directory and display it."
  (interactive)
  (image-dired default-directory))

(defun dired-open-feh ()
  "Make a preview buffer for all images in current directory and display it."
  (interactive)
  (let ((cmd "feh -F -Z * &" ))
    (message cmd)
    (dired-do-shell-command cmd nil (list (dired-get-file-for-visit)))
    ))

(eval-after-load 'dired
  '(progn (define-key dired-mode-map "." 'dired-hide-dotfiles-mode)
          (define-key dired-mode-map "," 'dired-hide-details-mode)
          (define-key dired-mode-map (kbd "C-i") 'image-dired-here)
          (define-key dired-mode-map (kbd "å") 'dired-open-feh)))

;;;;;;;;;;
;; occur-mode
(defun my-occur-mode-keybindings ()
  (define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)
  (define-key occur-mode-map (kbd "n") 'next-logical-line)
  (define-key occur-mode-map (kbd "p") 'previous-logical-line))
(add-hook 'occur-mode-hook 'my-occur-mode-keybindings)

;;;;;;;;;;
;; wgrep
(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(provide 'my-keybindings)

;; my-keybindings.el ends here
