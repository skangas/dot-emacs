;;; C++

(defun my-c++-mode-customizations ()
  (my-coding-keys c++-mode-map)

  (setq c-default-style "gnu"
        c-basic-offset 2
        tab-width 2
        indent-tabs-mode nil
        backward-delete-function nil  ; do NOT expand tabs when deleting them
        c-toggle-auto-state 0         ; Do *not* start newline on certain characters
        c-toggle-hungry-state 1)      ; Do *not * delete to next non-whitespace

  (setq paragraph-start "[ 	]*\\(//+\\|\\**\\)[ 	]*$\\|^\f")

  (c-set-offset 'arglist-intro 2)
  (c-set-offset 'arglist-cont-nonempty 2)

  (flyspell-prog-mode))

(add-hook 'c++-mode-hook 'my-c++-mode-customizations)

(provide 'init-coding-cpp)

;; init-coding-c.el ends here
