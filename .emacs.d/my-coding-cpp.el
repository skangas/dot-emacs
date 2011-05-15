;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

(setq compilation-read-command nil)

(add-hook 'c++-mode-hook 'my-c++-mode-customizations)
;; (defun my-c++-mode-customizations ()
;;   (my-coding-keys c++-mode-map)

;;   (setq c-default-style "linux"
;;         c-basic-offset 8
;;         tab-width 8
;;         indent-tabs-mode t
;;         backward-delete-function nil  ; do NOT expand tabs when deleting them
;;         c-toggle-auto-state 0         ; Do *not* start newline on certain characters
;;         c-toggle-hungry-state 1)      ; Do *not * delete to next non-whitespace

;;   (setq paragraph-start "[ 	]*\\(//+\\|\\**\\)[ 	]*$\\|^\f")

;;   ;; brackets should be at same indentation level as the statements they open
;;   (c-set-offset 'substatement-open '0))

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

;; run make if there is a Makefile, otherwise, run gcc
(add-hook 'c++-mode-hook
   (lambda ()
     (my-compile-runs-makefile-or-compiler
      (lambda (file)
        (concat "g++ -O2 -Wall -o " (file-name-sans-extension file)
                " " file)))))

(provide 'my-coding-cpp)

;; my-coding-c.el ends here
