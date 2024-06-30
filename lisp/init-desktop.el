;; Warn if there is no desktop.
(add-hook 'desktop-not-loaded-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (insert ";; WARNING: desktop not loaded - already in use\n\n"))))

(setq desktop-buffers-not-to-save
      (rx (or (seq bol "nn.a" (one-or-more (any "0-9")))
              ".log"
              "(ftp)"
              (seq bol "tags")
              (seq bol "TAGS")
              (seq ".emacs" (zero-or-more nonl))
              ".diary"
              ".newsrc-dribble"
              ".bbdb"
              )
          eol))

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
