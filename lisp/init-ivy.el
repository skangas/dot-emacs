;;; init-ivy.el                                                 -*- lexical-binding: t; -*-

(use-package flx                        ; mostly needed for ivy completion
  :ensure t
  :pin "melpa")

(use-package counsel
	     :ensure t
	     :pin "gnu"
	     :config
	     (global-set-key (kbd "M-x") 'counsel-M-x)
	       ;; Ivy-based interface to standard commands
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)

  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  ;; (global-set-key (kbd "C-c v") 'ivy-push-view)
  ;; (global-set-key (kbd "C-c V") 'ivy-pop-view)

  ;; Ivy-based interface to shell and system tools
  ;; (global-set-key (kbd "C-c c") 'counsel-compile)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  ;; (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (global-set-key (kbd "C-c w") 'counsel-wmctrl)

  (global-set-key (kbd "C-c b") 'counsel-bookmark)
  (global-set-key (kbd "C-c d") 'counsel-descbinds)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c o") 'counsel-outline)
  (global-set-key (kbd "C-c t") 'counsel-load-theme)
  (global-set-key (kbd "C-c F") 'counsel-org-file)

  ;; (global-set-key (kbd "C-c g") 'counsel-git)

  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

)

(use-package ivy
  :ensure t
  :pin "gnu"
  :diminish ivy-mode
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

  
  ;; Global bindings
  (global-set-key (kbd "C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "C-s") 'swiper-isearch)


  ;; Ivy-resume and other commands
  (global-set-key (kbd "C-c C-r") 'ivy-resume)

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
