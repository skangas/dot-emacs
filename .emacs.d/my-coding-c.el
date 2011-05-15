;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANSI-C

(defun my-c-mode-hook-customizations ()
  (c-set-offset 'arglist-intro '+))

;; (add-hook 'c-mode-hook 'my-c-mode-customizations)
(defun my-c-mode-customizations ()
  (interactive)
  (setq c-file-style "linux"
        c-basic-offset 4
        tab-width 4
        indent-tabs-mode t
        backward-delete-function nil  ; do NOT expand tabs when deleting them
        c-toggle-auto-state 0         ; Do *not* start newline on certain characters
        c-toggle-hungry-state 1)      ; Do *not * delete to next non-whitespace

  (setq paragraph-start "[      ]*\\(//+\\|\\**\\)[     ]*$\\|^\f")

  ;; "my-coding-keys" is defined above
  (my-coding-keys c-mode-base-map)

  ;; brackets should be at same indentation level as the statements they open
  (c-set-offset 'substatement-open '0))

;; (defun my-c-mode-customizations-gnu ()
;;   (interactive)
;;   (setq c-file-style "gnu"
;;         c-basic-offset 2
;;         tab-width 2
;;         indent-tabs-mode nil
;;         backward-delete-function nil  ; do NOT expand tabs when deleting them
;;         c-toggle-auto-state 0         ; Do *not* start newline on certain characters
;;         c-toggle-hungry-state 1)      ; Do *not * delete to next non-whitespace

;;   (setq paragraph-start "[      ]*\\(//+\\|\\**\\)[     ]*$\\|^\f")

;;   ;; "my-coding-keys" is defined above
;;   (my-coding-keys c-mode-base-map)

;;   (c-set-offset 'substatement-open '2))

;; (add-hook 'c-mode-hook 'my-c-mode-customizations)


;; compile command
(add-hook 'c-mode-hook
   (lambda ()
     (my-compile-runs-makefile-or-compiler
      (lambda (file)
        (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
                " " file)))))

(provide 'my-coding-c)

;; my-coding-c.el ends here
