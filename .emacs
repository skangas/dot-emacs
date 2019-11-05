;; -*- no-byte-compile: t -*-
;;
;; ~skangas/.emacs
;;

(setq message-log-max (* 20 message-log-max))
(setq max-specpdl-size (* 10 max-specpdl-size))
(setq max-lisp-eval-depth (* 10 max-lisp-eval-depth))

;; Temporarily raise garbage collection limit for initialization
(setq gc-cons-threshold (* 1024 1024 1024))
(defun my-lower-gc-cons-threshold ()
  ;; Revert back to something slightly bigger than the default
  (setq gc-cons-threshold 1000000)
  (remove-hook 'focus-out-hook #'my-lower-gc-cons-threshold)) 
(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer
             1
             nil
             #'my-lower-gc-cons-threshold)
            (add-hook 'focus-out-hook #'my-lower-gc-cons-threshold)))


;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-contrib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/predictive"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

;;; Packages and contrib.
(require 'init-package)

;; Create necessary directories
(dolist (dir '("~/.emacs.d/cache" "~/.emacs.d/cache/semanticdb"))
  (unless (file-directory-p dir)
    (make-directory dir)))


;;; Enable theme early to avoid flickering.
(use-package zenburn-theme
  :ensure t
  :config
  (when (version< "27" emacs-version)
    (load-theme 'zenburn t)))

;; General configuration.  (the order matters)
(require 'init-portability)
(require 'init-general)
;; (require 'init-compat)
(require 'init-emacs-server)
(require 'init-keybindings)
(require 'init-desktop)

;; Various features
(require 'init-auto-insert-mode)
(require 'init-elfeed)
(require 'init-hydra)
(require 'init-ivy)
(require 'init-mentor)
(require 'init-org-mode)
;; (require 'init-bbdb)
;; (require 'init-emms)
;; (require 'init-mu4e)
;; (require 'init-rcirc)
;; (require 'init-w3m)

(require 'init-mail)

;; Coding
(require 'init-coding-common)
(require 'init-coding-c)
(require 'init-coding-cedet)
;; (require 'init-coding-common-lisp)
(require 'init-coding-cpp)
(require 'init-coding-emacs-lisp)
(require 'init-coding-haskell)
;; (require 'init-coding-java)
(require 'init-coding-perl)
(require 'init-coding-php)
(require 'init-coding-python)
(require 'init-coding-ruby)
(require 'init-coding-scheme)

;; My code
(require 'sk-lisp)
(require 'sk-idom-article-length)
(require 'sk-misc)

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

;; Don't clutter .emacs with M-x customize stuff
(setq custom-file "~/.emacs.d/lisp/init-custom-file.el")
(load custom-file 'noerror)

;; Show current version in *scratch* buffer (this needs to be last to be on top)
;; and echo .emacs load time
(add-hook 'after-init-hook
          (lambda ()
            (insert (concat ";; " (substring (emacs-version) 0 14)))
            (when (not noninteractive)
              (insert (format " loaded in %s\n" (emacs-init-time))))
            (newline-and-indent)  (newline-and-indent)))
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
