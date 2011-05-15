;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common LISP

(add-hook 'lisp-mode-hook 'my-lisp-mode-customizations t)
(defun my-lisp-mode-customizations ()
  (my-coding-keys lisp-mode-base-map))

;; Scheme

(add-hook 'scheme-mode-hook 'my-scheme-mode-customizations t)
(defun my-scheme-mode-customizations ()
  (my-coding-keys scheme-mode-map))

(provide 'my-coding-common-lisp)

;; my-coding-common-lisp.el ends here
