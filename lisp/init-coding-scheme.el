;; (require 'quack)
;; (quack-install)

(defun my-scheme-mode-customizations ()
  ;; (setq quack-fontify-style 'emacs)
  (my-coding-keys scheme-mode-map))

(add-hook 'scheme-mode-hook 'my-scheme-mode-customizations t)

;; Geiser
(after 'geiser
  (setq geiser-default-implementation 'racket))

(provide 'init-coding-scheme)

;; my-coding-scheme.el ends here
