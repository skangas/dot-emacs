;;; init-ivy.el                                                 -*- lexical-binding: t; -*-

(use-package flx                        ; mostly needed for ivy completion
  :ensure t
  :pin "melpa-stable")

(use-package ivy
  :ensure t
  :pin "gnu"
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-virtual-abbreviate 'full    ; Show the full virtual file paths
        ;; ivy-extra-directories nil       ; default value: '("../" "./")
        ivy-extra-directories '("../" "./")
        ivy-format-function 'ivy-format-function-arrow
        ivy-height 15)

  ;; Global bindings
  (global-set-key (kbd "C-r") 'swiper-backward)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)

  ;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  ;; (global-set-key (kbd "C-c c") 'counsel-compile)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

  (global-set-key (kbd "C-c C-r") 'ivy-resume)

  ;; Minibuffer bindings
  (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word))

(provide 'init-ivy)
