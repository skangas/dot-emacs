(desktop-save-mode 1)

(setq desktop-save 'if-exists)
(setq desktop-dirname (expand-file-name "~/.emacs.d/cache"))
(add-to-list 'desktop-path desktop-dirname)

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Automatically save desktop when idle
;; (add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

;;; desktop-override-stale-locks.el begins here
;; (defun emacs-process-p (pid)
;;   "If pid is the process ID of an emacs process, return t, else nil.
;; Also returns nil if pid is nil."
;;   (when pid
;;     (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
;;       (when (file-exists-p cmdline-file)
;;         (with-temp-buffer
;;           (insert-file-contents-literally cmdline-file)
;;           (goto-char (point-min))
;;           (search-forward "emacs" nil t)
;;           pid)))))

;; (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
;;   "Don't allow dead emacsen to own the desktop file."
;;   (when (not (emacs-process-p ad-return-value))
;;     (setq ad-return-value nil)))
;;; desktop-override-stale-locks.el ends here

(provide 'my-desktop)

;; my-desktop.el ends here
