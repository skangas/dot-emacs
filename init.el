;;; init.el --- Emacs configuration  -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(setq message-log-max (* 10 message-log-max))
;; (setq max-specpdl-size (* 10 max-specpdl-size))
;; (setq max-lisp-eval-depth (* 10 max-lisp-eval-depth))


;; Debian specific
(when (and (equal system-name "joffe")
           (not (fboundp 'debian-startup)))

  ;; From /usr/share/doc/emacs-common/README.Debian.gz
  (setq debian-emacs-flavor 'emacs)

  ;; ;; Ugly hack to load Debian installed "elpa-*" packages.
  (load-file "/usr/share/emacs/site-lisp/debian-startup.el")

  ;; Workaround for Emacs < 27.1.
  ;; See /usr/share/emacs/site-lisp/dictionaries-common/debian-ispell.el
  ;; FIXME: Maybe not needed soon.
  (setq ispell-menu-map-needed nil)

  (debian-startup 'emacs)
  (let ((default-directory "/usr/share/emacs/site-lisp"))
    (load-file "/usr/share/emacs/site-lisp/subdirs.el")))

;; Make `load' prefer the newest version of a file.
(setq load-prefer-newer t)


;;; Package and use-package

(when (< emacs-major-version 27) (package-initialize))

;; Uncomment this if there are any problems with not finding packages:
;; (package-refresh-contents)

;; Configure MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Local packages
(dolist (dir '("~/wip/org-mode/lisp"
               "~/wip/mentor"
               "~/wip/url-scgi"))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)))

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

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

;; For the use-package `:diminish' keyword.
(use-package diminish :ensure t)

(use-package auto-compile
  :ensure t
  :disabled                             ; Led to some crashes and other weird issues.
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq load-prefer-newer t)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  ;; (defun my-inhibit-byte-compile ()
  ;;   (string-match "^/home/skangas/wip/emacs" buffer-file-name))
  ;; (add-hook 'auto-compile-inhibit-compile-hook 'my-inhibit-byte-compile)
  )

;; Create necessary directories
(dolist (dir '("~/.emacs.d/cache" "~/.emacs.d/cache/semanticdb"))
  (unless (file-directory-p dir)
    (make-directory dir)))

;;; Enable theme early to avoid flickering.
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (when (version< "27" emacs-version)
;;     (load-theme 'zenburn t)))

(ignore-errors
  (load-theme 'modus-operandi))

;; General configuration.  (the order matters)
(require 'init-portability)
(require 'init-general)
(require 'init-compat)
(require 'init-emacs-server)
(require 'init-keybindings)
;;(require 'init-desktop)

;; Various features
(require 'init-auto-insert-mode)
(require 'init-elfeed)
(require 'init-hydra)
(require 'init-ivy)
(require 'init-mentor)
(unless (eq system-type 'darwin)
  (require 'init-mail))
(require 'init-org-mode)

;; (require 'init-emms)
;; (require 'init-mu4e)
;; (require 'init-rcirc)
;; (require 'init-w3m)

;; Coding
(require 'init-coding-common)
(require 'init-lang-c)
(require 'init-lang-cpp)
(require 'init-lang-elisp)
(require 'init-lang-perl)
(require 'init-python)
;; (require 'init-lang-ruby)
;; (require 'init-lang-scheme)

;; My code
(require 'sk-lisp)
(require 'sk-idom-article-length)
(require 'sk-misc)

;; Don't clutter .emacs with M-x customize stuff
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file 'noerror)

;; Show current version in *scratch* buffer and echo .emacs load time
;; (this needs to be last to be on top)
(add-hook 'after-init-hook
          (lambda ()
            (insert (concat ";; " (substring (emacs-version) 0 14)))
            (when (not noninteractive)
              (insert (format " loaded in %s\n" (emacs-init-time))))
            (newline-and-indent)  (newline-and-indent)))

;; Enable some features
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'help-fns-edit-variable 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;;; init.el ends here
