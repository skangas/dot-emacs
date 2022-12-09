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

;; Some local packages
(dolist (p '("~/wip/org-mode/lisp"))
  (when (file-directory-p p)
    (add-to-list 'load-path p)))


;;; use-package

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

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
;;   (setq auto-compile-mode-line-counter t)
;;   ;; (defun my-inhibit-byte-compile ()
;;   ;;   (string-match "^/home/skangas/wip/emacs" buffer-file-name))
;;   ;; (add-hook 'auto-compile-inhibit-compile-hook 'my-inhibit-byte-compile)
;;   )

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

(provide 'init-package)
