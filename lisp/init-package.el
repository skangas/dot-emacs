;;; init-package.el                                                 -*- lexical-binding: t; -*-


;;; Configuration

;; ‘load’ prefers the newest version of a file.
(setq load-prefer-newer t)


;;; Package
(require 'package)
(when (< emacs-major-version 27)
  (package-initialize))

;; Uncomment this if we have any problems with not finding packages:
;; (package-refresh-contents)

;; Configure MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)


;;; use-package

;; Bootstrap use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Configure use-package
;(eval-when-compile
;  (require 'use-package))
(setq use-package-always-pin "melpa")

;; For use-package :diminish
(use-package diminish
  :ensure t
  :init
  (require 'diminish))

;; For use-package :bind
(use-package bind-key
  :ensure t
  :init
  (require 'bind-key))

;; Enable auto-compile

;; This lead to some crashes and other weird issues, disable:

;; (use-package auto-compile
;;   :ensure t
;;   :init
;;   (auto-compile-on-load-mode)
;;   (auto-compile-on-save-mode)
;;   (setq load-prefer-newer t)
;;   (setq auto-compile-display-buffer nil)
;;   (setq auto-compile-mode-line-counter t))

(provide 'init-package)
