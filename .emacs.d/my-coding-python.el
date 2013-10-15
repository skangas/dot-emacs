;; pymacs

(require 'python)
(require 'projectile)
(require 'pymacs)

(eval-after-load 'projectile
  (progn
    (add-hook 'python-mode-hook 'projectile-on)))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file)))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init))

(provide 'my-coding-python)

;; my-coding-python.el ends here
