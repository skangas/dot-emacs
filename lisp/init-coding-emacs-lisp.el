;; Emacs Lisp

;; (require 'eval-expr)
;; (eval-expr-install)

(defun sk/emacs-lisp-data-mode-hook ()
  (add-hook 'local-write-file-hooks #'check-parens))
(add-hook 'lisp-data-mode-hook #'sk/emacs-lisp-data-mode-hook)

(defun my-recompile-el ()
  (interactive)
  (when (and buffer-file-name
             (not (string-match "^/home/skangas/wip/emacs" buffer-file-name))
             (not (string-match "^/home/skangas/.emacs.d" buffer-file-name))
             (not (string-match "\\.dir-locals\\.el$"  buffer-file-name))
             (string-match "/.*\\.el$"  buffer-file-name)
                                        ;                     (string-match init-file-user buffer-file-name)) ;; XXX: doesn't work
             (file-newer-than-file-p buffer-file-name
                                     (concat buffer-file-name "c")))
    (when (file-exists-p (concat buffer-file-name ".elc"))
      (delete-file (concat buffer-file-name ".elc")))
    (byte-compile-file buffer-file-name)))

(defun my-emacs-lisp-mode-hook ()
  (my-coding-keys emacs-lisp-mode-map)

  ;; Abbreviate Emacs Lisp
  (setq mode-name "el")

  ;; automatically compile all .el files on save
  (add-hook 'after-save-hook #'my-recompile-el))

(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

(provide 'init-coding-emacs-lisp)
