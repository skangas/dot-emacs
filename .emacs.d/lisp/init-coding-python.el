;;; Python

(use-package python
  :config
  ;; flymake
  (when (load "flymake" t)
    (defun flymake-pylint-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "epylint" (list local-file)))))
  (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pylint-init)))

(eval-after-load 'projectile
  (progn
    (add-hook 'python-mode-hook 'projectile-mode)))

(provide 'init-coding-python)

;; init-coding-python.el ends here
