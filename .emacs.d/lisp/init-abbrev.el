;; (emacs)Abbrevs

;; Enable abbrev-mode
(setq default-abbrev-mode t
      save-abbrevs t
      abbrev-file-name "~/.emacs.d/abbrev_defs")

;; reads the abbreviations file on startup
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; Enable abbrev-mode in text and derived modes
(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; Several other modes

;; (dolist (hook '(erc-mode-hook
;;                 emacs-lisp-mode-hook)
;;   (add-hook hook (lambda () (abbrev-mode 1))))

(provide 'my-abbrev)
