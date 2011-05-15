(require 'quack)
(quack-install)

(add-hook 'scheme-mode-hook 'my-scheme-mode-customizations t)
(defun my-scheme-mode-customizations ()
  (setq quack-fontify-style 'emacs)
  (my-coding-keys scheme-mode-map))

(provide 'my-coding-scheme)

;; my-coding-scheme.el ends here
