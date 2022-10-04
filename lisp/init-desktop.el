(setq desktop-restore-eager 5)
(desktop-save-mode 1)

;; Warn if there is no desktop.
(add-hook 'desktop-not-loaded-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (insert ";; WARNING: desktop not loaded - already in use\n\n"))))

;; Use new 'guess value.
(if (fboundp 'desktop--load-locked-desktop-p) ; >= Emacs 28
    (setq desktop-load-locked-desktop 'check))

;; (setq desktop-restore-eager 10)

(setq desktop-save 'if-exists)
(setq desktop-dirname (expand-file-name "~/.emacs.d/cache"))
(add-to-list 'desktop-path desktop-dirname)

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))

;; Yes, ugly, but whatever.
;; (setq desktop-files-not-to-save-orig desktop-files-not-to-save)
;; (setq desktop-files-not-to-save
;;       (regexp-opt
;;        (list
;;         "\\`/mnt/usb/seed/"
;;         desktop-files-not-to-save-orig)))
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

(provide 'init-desktop)
