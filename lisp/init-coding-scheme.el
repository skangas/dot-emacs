;;; Scheme

;; (require 'quack)
;; (quack-install)
(defun my-scheme-mode-customizations ()
  ;; (setq quack-fontify-style 'emacs)
  (my-coding-keys scheme-mode-map))
(add-hook 'scheme-mode-hook 'my-scheme-mode-customizations t)

;; Geiser
(use-package geiser
  :ensure t
  :config
  (setq geiser-default-implementation 'racket))

(provide 'init-coding-scheme)
