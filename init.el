;; -*- no-byte-compile: t -*-
;;
;; ~skangas/.emacs
;;

;; Log .emacs start time
(defconst *emacs-start-time* (current-time))

;; Get this over with. Has to be a require.
(require 'cl)

;; Settings for MacOS
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)
(setq ns-right-alternate-modifier 'none)             ; use right alt for special characters

;; Fix path for MacOSX
(when (and (memq window-system '(mac ns))
           (fboundp 'exec-path-from-shell-initialize))
  (exec-path-from-shell-initialize))

;; Package
(package-initialize)
(setq load-prefer-newer t)

;; Configure ELPA
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-contrib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/predictive"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-pin "melpa-stable")

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package zenburn-theme        ; enable theme early
  :ensure t)

(use-package auto-compile
  :ensure t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t))

;; Various configuration
(setq message-log-max 1024) ;; do this first
(setq max-specpdl-size 15600)
(setq max-lisp-eval-depth 9000)

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
(require 'init-coding-elisp)
(require 'init-coding-haskell)
(require 'init-coding-java)
(require 'init-coding-perl)
(require 'init-coding-php)
(require 'init-coding-python)
(require 'init-coding-scheme)

(require 'init-desktop)

;; (require 'init-google-translate)

;; My code
(require 'sk-lisp)
(require 'sk-idom-article-length)

;; Various packages
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

(when (condition-case nil
          (require 'completion-ui)
        (error nil))
  (eval-after-load "cc-mode"
    '(progn
       (define-key c-mode-map [?\M-\t] 'complete-semantic)
       (define-key c++-mode-map [?\M-\t] 'complete-semantic))))

;; Don't clutter .emacs with M-x customize stuff
(setq custom-file "~/.emacs.d/my-custom-file.el")
(load custom-file 'noerror)

;; Workaround for broken visual bell on OSX El Capitain
(when (eq system-type 'darwin)
  (setq visible-bell nil)
  (setq ring-bell-function
        (lambda ()
          (invert-face 'mode-line)
          (run-with-timer 0.1 nil 'invert-face 'mode-line))))

;; Show current version in *scratch* buffer (this needs to be last to be on top)
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

;; Echo .emacs load time

