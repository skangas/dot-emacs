;;; init-coding-emacs-lisp.el --- Emacs Lisp
;;; Commentary:
;;; Code:

(use-package elisp-mode :ensure nil
  :defer t
  :diminish (emacs-lisp-mode . "el")
  :init
  (defun my-emacs-lisp-mode-hook ()
    (my-coding-keys emacs-lisp-mode-map)
    (add-hook 'after-save-hook #'my-recompile-el))
  :hook (emacs-lisp-mode . my-emacs-lisp-mode-hook))

(use-package checkdoc :defer t
  :config
  (push "org-mode" checkdoc-symbol-words))

(use-package nameless
  :diminish
  :hook emacs-lisp-mode)

(use-package package-lint :defer t)

(add-hook 'lisp-data-mode-hook
          (lambda ()
            (add-hook 'write-file-functions #'check-parens nil t)))

(defun my-recompile-el ()
  "Recompile Emacs Lisp files automatically, but not in some directories."
  (interactive)
  (rx-let ((home (: bos "/" (or "home" "Users") "/skangas")))
    (when (and buffer-file-name
               (not (string-match (rx home "/wip/emacs") buffer-file-name))
               (not (string-match (rx home "/.emacs.d") buffer-file-name))
               (not (string-match "\\.dir-locals\\.el$"  buffer-file-name))
               (string-match "/.*\\.el$"  buffer-file-name)
               ;; (string-match init-file-user buffer-file-name)) ;; XXX: doesn't work
               (file-newer-than-file-p buffer-file-name
                                       (concat buffer-file-name "c")))
      (when (file-exists-p (concat buffer-file-name ".elc"))
        (delete-file (concat buffer-file-name ".elc")))
      (byte-compile-file buffer-file-name))))

(provide 'init-lang-elisp)

;;; init-lang-elisp.el ends here
