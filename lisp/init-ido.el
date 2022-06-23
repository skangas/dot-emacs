;;; init-ido.el                                                 -*- lexical-binding: t; -*-

(use-package ido
  :config
  (ido-mode 0)
  (ido-everywhere 0)

  (setq ido-enable-flex-matching t
        ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-work-directory-list '("~/" "~/org" "~/src")
        ido-case-fold t                 ; Be case-insensitive
        ido-max-directory-size 100000   ; Avoid [Too Big] messages
        ;; display matches vertically
        ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                                " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  ;; http://whattheemacsd.com/setup-ido.el-02.html
  (defun my-ido-go-straight-home ()
    ;; Go straight home
    (define-key ido-file-completion-map
                (kbd "~")
                (lambda ()
                  (interactive)
                  (if (looking-back "/")
                      (insert "~/")
                    (call-interactively 'self-insert-command)))))
  (add-hook 'ido-setup-hook 'my-ido-go-straight-home))

;; ;; Disabled.  It is slow and sometimes broken.
;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   (setq ido-cr+-auto-update-blacklist t))

(provide 'init-ido)
