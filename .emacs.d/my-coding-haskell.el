;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell

;; mina grejer
(defun my-haskell-newline-and-indent ()
  (interactive)
  "The default behavior of 'newline-and-indent in haskell-mode is annoying.
This will run newline-and-indent, and then indent once more."
  (newline-and-indent)
  (indent-for-tab-command)
  (indent-for-tab-command))

(add-hook 'haskell-mode-hook 'my-haskell-customizations t)
(defun my-haskell-customizations ()
  "haskell-mode customizations that must be done after haskell-mode loads"
  (defvar multiline-flymake-mode nil)
  (defvar flymake-split-output-multiline nil)
  (define-key haskell-mode-map (kbd "RET") 'my-haskell-newline-and-indent)
  (flymake-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "ghc"
    	(list "--make" "-fbyte-code"
    	      (concat "-i"base-dir)  ;;; can be expanded for additional -i options as in the Perl script
    	      source)))

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'haskell-refac-mode)
;; (add-hook 'haskell-mode-hook 'hs-lint-mode-hook)

;; show error below
(when (fboundp 'resize-minibuffer-mode) ; for old emacs
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil))


;; TODO what does this do?
;; (defvar multiline-flymake-mode nil)
;; (defvar flymake-split-output-multiline nil)

;; this needs to be advised as flymake-split-string is used in other places and
;; I don't know of a better way to get at the caller's details
(defadvice flymake-split-output
  (around flymake-split-output-multiline activate protect)
  (if multiline-flymake-mode
      (let ((flymake-split-output-multiline t))
        ad-do-it)
    ad-do-it))

(defadvice flymake-split-string
  (before flymake-split-string-multiline activate)
  (when flymake-split-output-multiline
    (ad-set-arg 1 "^\\s *$")))

(add-hook
 'haskell-mode-hook
 '(lambda ()
     	;;; use add-to-list rather than push to avoid growing the list for every Haskell file loaded
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
    (add-to-list 'flymake-err-line-patterns
                 '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
                   1 2 3 4))
    (set (make-local-variable 'multiline-flymake-mode) t)))

(defun credmp/flymake-display-err-minibuf () 
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;; bind the above function to C-c d
(add-hook
 'haskell-mode-hook
 '(lambda ()
    (define-key haskell-mode-map "\C-cd"
      'credmp/flymake-display-err-minibuf)))

(add-hook 'haskell-mode-hook 'haskell-simple-indent-mode)

(provide 'my-coding-haskell)

;; my-coding-haskell.el ends here
