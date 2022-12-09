;;; init-portability.el                                                 -*- lexical-binding: t; -*-


;;; macOS portability.
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'super)
  (setq ns-right-alternate-modifier 'none) ; use right alt for special characters

  (use-package exec-path-from-shell
    :ensure t
    :init
    (exec-path-from-shell-initialize))

  (use-package ns-auto-titlebar
    :ensure t
    :init
    (ns-auto-titlebar-mode))

  ;; Workaround for broken visual bell on OSX El Capitain
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda ()
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))


;;; Windows portability
(when (eq system-type 'windows-nt)
  ;; Hack to get my configuration running at work
  (when load-file-name
    (setenv "HOME" (file-name-directory load-file-name))))

(provide 'init-portability)
