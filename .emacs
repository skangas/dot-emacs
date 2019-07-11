;; -*- no-byte-compile: t -*-
;;
;; ~skangas/.emacs
;;

;; Log .emacs start time
(defconst *emacs-start-time* (current-time))

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

;; Get this over with. Has to be a require.
(require 'cl)

;; Package
(require 'package)
(package-initialize)
;; Uncomment this if we have any problems with not finding packages:
;; (package-refresh-contents)
(setq load-prefer-newer t)

;; Configure ELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-contrib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/predictive"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

;; Bootstrap use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Configure use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-pin "melpa-stable")
(use-package diminish ; for use-package :diminish
  :ensure t
  :init
  (require 'diminish))
(use-package bind-key ; for use-package :bind
  :ensure t
  :init
  (require 'bind-key))

;; Enable theme early
(use-package zenburn-theme
  :ensure t
  :config
  (when (version< "27" emacs-version)
    (load-theme 'zenburn)))

;; Enable auto-compile
(use-package auto-compile
  :ensure t
  :init
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq load-prefer-newer t)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

;; Various configuration
(setq message-log-max 1024) ;; do this first
(setq max-specpdl-size 15600)
(setq max-lisp-eval-depth 9000)

;; Settings for MacOS
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)
(setq ns-right-alternate-modifier 'none)             ; use right alt for special characters

;; Fix path for MacOSX
(when (memq window-system '(mac ns))  
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

;; Hack to get my configuration running at work (Windows)
(when (eq system-type 'windows-nt)
  (when load-file-name
    (setenv "HOME" (file-name-directory load-file-name))))

;; Create necessary directories
(dolist (dir '("~/.emacs.d/cache" "~/.emacs.d/cache/semanticdb"))
  (unless (file-directory-p dir)
    (make-directory dir)))

;; Require my configuration files
(require 'init-general)
(require 'init-compat)
(require 'init-emacs-server)
(require 'init-keybindings)

(require 'init-auto-insert-mode)
;; (require 'init-bbdb)
;; (require 'init-emms)
(require 'init-org-mode)
;; (require 'init-rcirc)
(require 'init-tramp)
;; (require 'init-w3m)

;; Coding
(require 'init-coding-common)
(require 'init-coding-c)
(require 'init-coding-cedet)
(require 'init-coding-common-lisp)
(require 'init-coding-cpp)
(require 'init-coding-emacs-lisp)
(require 'init-coding-haskell)
(require 'init-coding-java)
(require 'init-coding-perl)
(require 'init-coding-php)
(require 'init-coding-python)
(require 'init-coding-ruby)
(require 'init-coding-scheme)

(require 'init-desktop)
(require 'init-hydra)

;; My code
(require 'sk-lisp)
(require 'sk-idom-article-length)

;; Various packages
(use-package xml-rpc
  :ensure t)
;; (setq mentor-rtorrent-external-rpc "http://localhost/our-RPC2")
;; (setq mentor-rtorrent-external-rpc "scgi:///~/.rtorrent-session/rpc.socket")
(setq mentor-rtorrent-external-rpc "scgi://127.0.0.1:5000")
(defun my-mentor ()
  (interactive)
  (when (require 'mentor nil t)
    ;; (setq mentor-highlight-enable t)
    ;; (setq mentor-view-columns
    ;;       '(((mentor-torrent-get-state) -3 "State")
    ;;         ((mentor-torrent-get-progress) -3 "Cmp")
    ;;         (name -40 "Name")
    ;;         ((mentor-torrent-get-speed-up) -6 "Up")
    ;;         ((mentor-torrent-get-speed-down) -6 "Down")
    ;;         ((mentor-torrent-get-size) -15 "     Size")
    ;;         (message -40 "Message")
    ;;         (directory -100 "Directory")
    ;;         (tied_to_file -80 "Tied file name")))
    ))

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

;; Don't clutter .emacs with M-x customize stuff
(setq custom-file "~/.emacs.d/lisp/init-custom-file.el")
(load custom-file 'noerror)

;; Workaround for broken visual bell on OSX El Capitain
(when (eq system-type 'darwin)
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda ()
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Show current version in *scratch* buffer (this needs to be last to be on top)
;; and echo .emacs load time
(add-hook 'after-init-hook
          (lambda ()
            (insert (concat ";; " (substring (emacs-version) 0 14) ".    "))
            (when (not noninteractive)
              (insert (format
                       " -- .emacs loaded in %d.2s\n"
                       (destructuring-bind (hi lo ms ps) (current-time)
                         (- (+ hi lo) (+ (first *emacs-start-time*)
                                         (second *emacs-start-time*)))))))
            (newline-and-indent)  (newline-and-indent)))


