;; stuff that needs to be executed last

;; Show current version (this needs to be last to be on top)
(defun my-welcome-message ()
  (insert (concat ";; " (substring (emacs-version) 0 16) "."))
  (newline-and-indent)  (newline-and-indent))
(add-hook 'after-init-hook 'my-welcome-message)

(provide 'my-z-end)

;; my-z-end.el ends here
