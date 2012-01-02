;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-lisp-mode

(defun my-emacs-lisp-mode-hook ()
  (my-coding-keys emacs-lisp-mode-map)

  ;; automatically compile all .el files on save
  (add-hook 'after-save-hook 'my-recompile-el)
  (defun my-recompile-el ()
    (interactive)
    (let ((byte-compile-warnings '(unresolved)))
      (when (and buffer-file-name
                 (string-match "/.*\\.el$"  buffer-file-name)
;                     (string-match init-file-user buffer-file-name)) ;; XXX: doesn't work
                 (file-newer-than-file-p buffer-file-name
                                         (concat buffer-file-name "c")))
        (when (file-exists-p (concat buffer-file-name ".elc"))
          (delete-file (concat buffer-file-name ".elc")))
        (byte-compile-file buffer-file-name)))))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(provide 'my-coding-elisp)

;; my-coding-elisp.el ends here
