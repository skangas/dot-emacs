;; Emacs Lisp

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

;; (require 'eval-expr)
;; (eval-expr-install)

(use-package nameless
  :ensure t
  :diminish nameless-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'nameless-mode))

(use-package macrostep
  :ensure t)

(use-package suggest
  :ensure t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; ;; Not using this for now.
;; (add-hook 'ielm-mode-hook 'enable-paredit-mode)

(defun my-emacs-lisp-mode-hook ()
  (my-coding-keys emacs-lisp-mode-map)

  ;; Abbreviate Emacs Lisp
  (setq mode-name "el")

  ;; automatically compile all .el files on save
  (add-hook 'after-save-hook 'my-recompile-el)
  (defun my-recompile-el ()
    (interactive)
    (let ((byte-compile-warnings '(unresolved)))
      (when (and buffer-file-name
                 (not (string-match "^/home/skangas/wip/emacs" buffer-file-name))
                 (not (string-match "\\.dir-locals\\.el$"  buffer-file-name))
                 (string-match "/.*\\.el$"  buffer-file-name)
;                     (string-match init-file-user buffer-file-name)) ;; XXX: doesn't work
                 (file-newer-than-file-p buffer-file-name
                                         (concat buffer-file-name "c")))
        (when (file-exists-p (concat buffer-file-name ".elc"))
          (delete-file (concat buffer-file-name ".elc")))
        (byte-compile-file buffer-file-name)))))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(provide 'init-coding-emacs-lisp)

;; init-coding-emacs-lisp.el ends here
