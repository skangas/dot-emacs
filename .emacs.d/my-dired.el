(require 'dired)
(require 'dired-x) ;; require immediately to provide C-x C-j

(setq dired-listing-switches "-lAh"  ;; Use human sizes
      dired-dwim-target t            ;; Try to guess a default target directory
      dired-isearch-filenames 'dwim) ;; Search filenames only

;;; Toggle showing dot-files using "."
(define-minor-mode dired-hide-dotfiles-mode
  ""
  :lighter " Hide"
  :init-value nil
  (if (not (eq major-mode 'dired-mode))
      (progn 
	(error "Doesn't seem to be a Dired buffer")
	(setq dired-hide-dotfiles-mode nil))
    (if dired-hide-dotfiles-mode
	(setq dired-actual-switches "-lh")
      (setq dired-actual-switches "-lAh"))
    (revert-buffer)))
(define-key dired-mode-map "." 'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook (lambda () (dired-hide-dotfiles-mode 1)))

(provide 'my-dired)

;; my-dired.el ends here
