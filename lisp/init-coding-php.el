;; PHP

(use-package php-mode
  :ensure t
  :mode ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
  :config
  (defun my-php-mode-customizations ()
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "php -l " file)))

    (setq c-basic-offset 4
          tab-width 4
          indent-tabs-mode nil          ; No tabs - only spaces
          backward-delete-function nil  ; do NOT expand tabs when deleting them
          ))
  (add-hook 'php-mode-hook 'my-php-mode-customizations))

(provide 'init-coding-php)
