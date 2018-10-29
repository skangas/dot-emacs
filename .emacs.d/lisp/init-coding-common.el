;;; General coding

(use-package auto-complete
  :ensure t
  :defer 30
  :config
  (ac-config-default)
  (setq ac-quick-help-delay 1.0))

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package flymake
  :config
  (defun my-flymake-show-next-error ()
     (interactive)
     (flymake-goto-next-error)
     (flymake-popup-current-error-menu))
  (add-hook 'flymake-mode-hook
            (local-set-key (kbd "C-c C-e") 'my-flymake-show-next-error)))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))

(use-package paredit
  :ensure t
  :config
  (autoload 'paredit-mode "paredit"
    "Minor mode for pseudo-structurally editing Lisp code." t)
  (defun my-enable-paredit-mode ()
    (paredit-mode +1))
  (add-hook 'emacs-lisp-mode-hook       'my-enable-paredit-mode)
  (add-hook 'lisp-mode-hook             'my-enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'my-enable-paredit-mode)
  (add-hook 'scheme-mode-hook           'my-enable-paredit-mode)
  
  ;; make eldoc aware of paredit
  (eval-after-load 'eldoc
    '(progn (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round))))

(use-package projectile
  :ensure t
  :pin "melpa"
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t
        projectile-globally-ignored-file-suffixes '(".elc")))

(use-package smartparens
  :ensure t
  :config
  (add-hook 'enh-ruby-mode-hook 'smartparens-mode))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :interpreter ("node" . web-mode)
  :ensure t)

(use-package yasnippet
  :ensure t
  :defer 20
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :defer 30)

(defun my-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'emacs-lisp-mode-hook 'my-pretty-lambda)
(add-hook 'ielm-mode-hook 'my-pretty-lambda)
(add-hook 'lisp-interaction-mode-hook 'my-pretty-lambda)
(add-hook 'scheme-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Shared bindings
(defun my-coding-keys (map)
  "Sets the key bindings which shall be available in all programming
  languages. Argument MAP is the local keymap (e.g. cperl-mode-map)."
  (define-key map (kbd "RET")                 'newline-and-indent)
  (define-key map (kbd "M-?")                 'indent-region))

;; Mark special words
(defun my-highlight-special-words ()
  (font-lock-add-keywords nil
                  '(("\\<\\(XXX\\|FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'c-mode-common-hook 'my-highlight-special-words)
(set-face-underline 'font-lock-warning-face "yellow")

(setq glasses-separate-parentheses-p nil)
;; (projectile ido-flx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gud

;Add color to the current GUD line
(defvar my-gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov my-gud-overlay)
         (bf (gud-find-file (ad-get-arg 0))))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    (current-buffer)))))
(defun my-gud-kill-buffer ()
  (if (eq major-mode 'gud-mode)
      (delete-overlay my-gud-overlay)))
(add-hook 'kill-buffer-hook 'my-gud-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile command

(setq compile-command "make -k -j5 ")

;; this function is used to set up compile command in both c and c++ conf
(defun my-compile-runs-makefile-or-compiler (create-compiler-command)
  "Configure compile command to run Makefile if it exists, or
otherwise use the compiler command given by passed in
compiler-command."
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (when (buffer-file-name)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (funcall create-compiler-command file))))))

(add-hook 'c-mode-hook
   (lambda ()
     (my-compile-runs-makefile-or-compiler
      (lambda (file)
        (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
                " " file)))))

(add-hook 'c++-mode-hook
   (lambda ()
     (my-compile-runs-makefile-or-compiler
      (lambda (file)
        (concat "g++ -O2 -Wall -o " (file-name-sans-extension file)
                " " file)))))

(provide 'init-coding-common)

;; init-coding.el ends here
