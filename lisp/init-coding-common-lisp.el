;; Common LISP

(add-hook 'lisp-mode-hook 'my-lisp-mode-customizations t)
(defun my-lisp-mode-customizations ()
  (my-coding-keys lisp-mode-base-map))

(provide 'init-coding-common-lisp)
