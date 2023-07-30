;;; init-ivy.el                                                 -*- lexical-binding: t; -*-

(use-package flx                        ; mostly needed for ivy completion
  :pin "melpa")

(use-package counsel
  :pin "gnu"
  :config
  (bind-key "M-x" 'counsel-M-x)
  ;; Ivy-based interface to standard commands
  (bind-key "M-x" 'counsel-M-x)
  (bind-key "C-x C-f" 'counsel-find-file)
  ;; (bind-key "M-y" 'counsel-yank-pop)
  (bind-key "C-h f" 'counsel-describe-function)

  ;; (bind-key "<f1> f" 'counsel-describe-function)
  ;; (bind-key "<f1> v" 'counsel-describe-variable)
  ;; (bind-key "<f1> l" 'counsel-find-library)
  ;; (bind-key "<f2> i" 'counsel-info-lookup-symbol)
  ;; (bind-key "<f2> u" 'counsel-unicode-char)
  ;; (bind-key "<f2> j" 'counsel-set-variable)
  ;; (bind-key "C-x b" 'ivy-switch-buffer)
  ;; (bind-key "C-c v" 'ivy-push-view)
  ;; (bind-key "C-c V" 'ivy-pop-view)

  ;; Ivy-based interface to shell and system tools
  ;; (bind-key "C-c c" 'counsel-compile)
  (bind-key "C-c g" 'counsel-git)
  (bind-key "C-c j" 'counsel-git-grep)
  (bind-key "C-c L" 'counsel-git-log)
  (bind-key "C-c k" 'counsel-rg)
  (bind-key "C-c m" 'counsel-linux-app)
  ;; (bind-key "C-c n" 'counsel-fzf)
  (bind-key "C-x l" 'counsel-locate)
  (bind-key "C-c J" 'counsel-file-jump)
  (bind-key "C-S-o" 'counsel-rhythmbox)
  (bind-key "C-c w" 'counsel-wmctrl)

  (bind-key "C-c b" 'counsel-bookmark)
  (bind-key "C-c d" 'counsel-descbinds)
  (bind-key "C-c g" 'counsel-git)
  (bind-key "C-c o" 'counsel-outline)
  (bind-key "C-c t" 'counsel-load-theme)
  (bind-key "C-c F" 'counsel-org-file)

  ;; (bind-key "C-c g" 'counsel-git)

  (bind-key "C-c k" 'counsel-ag)
  (bind-key "C-x l" 'counsel-locate)
  ;; (bind-key "C-S-o" 'counsel-rhythmbox)

  )

(use-package ivy
  :pin "gnu"
  :diminish ivy-mode
  :bind (("C-r" . swiper-isearch-backward)
         ("C-s" . swiper-isearch)
         ("C-c C-r" . ivy-resume))
  :config
  (ivy-mode 1)

  ;; When non-nil, add recent files and/or bookmarks to `ivy-switch-buffer'.
  (setq ivy-use-virtual-buffers t)

  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 10)

  ;; ivy-extra-directories nil       ; default value: '("../" "./")
  (setq ivy-extra-directories '("../" "./"))

  (setq ivy-format-function 'ivy-format-function-arrow)

  ;; Show the full virtual file paths
  (setq ivy-virtual-abbreviate 'full)

  ;; Minibuffer bindings
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)

  (setq ivy-initial-inputs-alist '((counsel-minor . "^+")
                                   (counsel-package . "^+ ")
                                   (org-refile . "^")
                                   (org-agenda-refile . "^")
                                   (org-capture-refile . "^")
                                   (counsel-M-x . "")
                                   (counsel-describe-function . "")
                                   (counsel-describe-variable . "^")
                                   (counsel-org-capture . "^")
                                   (Man-completion-table . "^")
                                   (woman . "^")))

  )


(provide 'init-ivy)
