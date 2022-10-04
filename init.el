;; -*- no-byte-compile: t -*-
;;
;; init.el
;;

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

;;; Packages and contrib.
(require 'init-package)

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

;; (require 'init-bbdb)
;; (require 'init-emms)
;; (require 'init-ido)
;; (require 'init-mu4e)
;; (require 'init-rcirc)
;; (require 'init-w3m)

;; Coding
(require 'init-coding-c)
(require 'init-coding-common)
(require 'init-coding-cpp)
(require 'init-coding-emacs-lisp)
(require 'init-coding-perl)
(require 'init-coding-php)
(require 'init-coding-python)
;; (require 'init-coding-cedet)
;; (require 'init-coding-common-lisp)
;; (require 'init-coding-haskell)
;; (require 'init-coding-java)
;; (require 'init-coding-ruby)
;; (require 'init-coding-scheme)

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
(put 'list-timers 'disabled nil)
(put 'help-fns-edit-variable 'disabled nil)
