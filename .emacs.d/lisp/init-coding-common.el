;;; General coding

;; TRYING OUT COMPANY INSTEAD
;; (use-package auto-complete
;;   :ensure t
;;   :defer 30
;;   :config
;;   (ac-config-default)
;;   (setq ac-quick-help-delay 2.0))

(setq compilation-scroll-output t)

(use-package company
  :ensure t
  :pin "gnu"
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package debbugs
  :ensure t
  :pin "gnu"
  :config
  (setq debbugs-gnu-emacs-current-release "27.1")
  (setq debbugs-gnu-branch-directory "~/wip/emacs26")
  (setq debbugs-gnu-trunk-directory "~/wip/emacs"))

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package elide-head
  :config
  (add-hook 'prog-mode-hook 'elide-head))

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
  :bind (("C-c g" . magit-dispatch))
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-repository-directories '(("~/wip/emacs/" . 0))))

(use-package markdown-mode
  :ensure t
  :defer 300 ; I rarely use this
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

(use-package smerge-mode
  :after hydra

  ;; This hydra taken from
  ;; https://github.com/alphapapa/unpackaged.el#hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)
                                     (recenter-top-bottom)))))

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

(use-package ws-butler ; Automatically trim whitespace on save.
  :ensure t
  :config
  ;; (setq ws-butler-convert-leading-tabs-or-spaces t)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

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

;; Disabled for now.
;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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
