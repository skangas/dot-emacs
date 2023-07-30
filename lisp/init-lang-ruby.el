;;; init-coding-ruby.el --- Ruby programming
;;; Commentary:
;;; Code:

;; Necessary setup:
;;  Install and use rvm
;;  gem install pry

(add-to-list 'auto-mode-alist '("Gemfile\\.lock$" . conf-mode))

;; We never want to edit Rubinius bytecode or MacRuby binaries
(add-to-list 'completion-ignored-extensions ".rbc")
(add-to-list 'completion-ignored-extensions ".rbo")

(defun my-ruby-mode-hook ()
  (setq-local ac-stop-words '("if" "end" "else" "do" "def")))

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :diminish "Ruby"
  :config
  ;; (add-to-list 'ac-modes 'enh-ruby-mode)
  (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile$" . enh-ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile$" . enh-ruby-mode))
  (add-hook 'enh-ruby-mode-hook 'my-ruby-mode-hook))

(use-package projectile-rails
  :pin "melpa"
  :config
  (projectile-rails-global-mode))

;; FIXME: This was last updated in 2015.  Still relevant?

(use-package rinari
  :disable t
  :pin "melpa"
  :config
  (add-hook 'enh-ruby-mode-hook 'rinari-minor-mode)
  ;; ;; Disabled for now to not clutter my minor-modes
  ;; (global-rinari-mode)
  )

(use-package robe
  :pin "melpa"
  :diminish "Robe"
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'enh-robe-mode-hook 'ac-robe-setup)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  ;; Run rvm first
  (when (fboundp 'rvm-activate-corresponding-ruby)
    (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
      (rvm-activate-corresponding-ruby))))

;; (use-package ruby-end
;;   :config
;;   (add-hook 'ruby-mode-hook 'ruby-end-mode)
;;   (add-hook 'enh-ruby-mode-hook 'ruby-end-mode))

(use-package ruby-test-mode
  :pin "melpa"
  :diminish "RT"
  :config
  (add-hook 'enh-ruby-mode-hook 'ruby-test-mode)
  (add-hook 'ruby-mode-hook 'ruby-test-mode))

(use-package inf-ruby
  :defer t)

;; Compare to ruby-compilation package
(use-package minitest
  :diminish "MT"
  :config
  (add-hook 'ruby-mode-hook 'minitest-mode))

;; (use-package rvm
;;   :config
;;   (rvm-use-default))

(provide 'init-lang-ruby)

;;; init-lang-ruby.el ends here
