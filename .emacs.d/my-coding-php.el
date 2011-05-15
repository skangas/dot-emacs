;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP

(autoload 'php-mode "php-mode" "PHP Mode." t)
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php[s34]?\\'" . php-mode))

(eval-after-load "php-mode"
  '(progn
     (defun my-php-mode-customizations ()
       (set (make-local-variable 'compile-command)
            (let ((file (file-name-nondirectory buffer-file-name)))
              (concat "php -l " file)))
       (setq c-basic-offset 4
             tab-width 4
             indent-tabs-mode nil ; No tabs - only spaces
             backward-delete-function nil ; do NOT expand tabs when deleting them
             ))
     (add-hook 'php-mode-hook 'my-php-mode-customizations)))

(provide 'my-coding-php)

;; my-coding-php.el ends here
