;;; early-init.el                                                 -*- lexical-binding: t; -*-

;; Font
(unless (eq window-system 'ns)
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-14")))
(setq-default line-spacing 1)

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-contrib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
;; (add-to-list 'load-path (expand-file-name "~/wip/org-mode/contrib"))
;; (add-to-list 'load-path (expand-file-name "~/wip/org-mode/lisp"))

(when (file-readable-p "~/.emacs-secrets.el")
  (load-file "~/.emacs-secrets.el"))

;; Temporarily raise garbage collection limit for initialization

(defvar my/backup-gc-cons-threshold gc-cons-threshold)

(defun my/lower-gc-cons-threshold ()
  "Revert back to something slightly bigger than the default."
  (setq gc-cons-threshold (+ my/backup-gc-cons-threshold 200000))
  ;; `focus-out-hook' is replaced with `after-focus-change-function' but I don't
  ;; bother with changing it right now.
  (remove-hook 'focus-out-hook #'my/lower-gc-cons-threshold))

(add-hook 'after-init-hook
          (lambda ()
            (run-with-idle-timer 3 nil #'my/lower-gc-cons-threshold)
            (add-hook 'focus-out-hook #'my/lower-gc-cons-threshold)))

(setq gc-cons-threshold (* 1024 gc-cons-threshold))
