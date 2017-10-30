;; -*- no-byte-compile: t -*-
;;
;; ~skangas/.emacs
;;

(defconst *emacs-start-time* (current-time))

;; Get this over with. Has to be a require.
(require 'cl)

;; various stuff 
(setq message-log-max 1024) ;; do this first
(setq max-specpdl-size 15600)
(setq max-lisp-eval-depth 9000)

;;; Loading packages
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))

;; (let ((d1 (car  (last (file-expand-wildcards "~/.emacs.d/elpa/auto-compile-20*"))))
;;       (d2 ".emacs.d/elpa/packed-20130502.2340/"))
;;   (when (and (file-directory-p d1)
;; 	     (file-directory-p d2))
;;     (add-to-list 'load-path d1)
;;     (add-to-list 'load-path d2)
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode 1)
;;     (auto-compile-on-save-mode 1)))


;; work-around for Emacs < 23.2
(when (< emacs-major-version 24)
  (require 'warnings))

;; Hack to get my configuration running at work (Windows)
(when (eq system-type 'windows-nt)
  (when load-file-name
    (setenv "HOME" (file-name-directory load-file-name))))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-contrib"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

;; Create necessary directories
(dolist (dir '("~/.emacs.d/cache" "~/.emacs.d/cache/semanticdb"))
  (unless (file-directory-p dir)
    (make-directory dir)))

;; Require my configuration files
(require 'my-general)
(require 'my-emacs-server)
(require 'my-keybindings)

(require 'my-abbrev)
(require 'my-auto-insert-mode)
(require 'my-bbdb)
(require 'my-dired)
;; (require 'my-emms)
(require 'my-org-mode)
;; (require 'my-rcirc)
(require 'my-tramp)
(require 'my-w3m)

(require 'my-coding)
(require 'my-cedet)
(require 'my-coding-c)
;; (require 'my-coding-common-lisp)
(require 'my-coding-cpp)
(require 'my-coding-elisp)
(require 'my-coding-haskell)
(require 'my-coding-java)
(require 'my-coding-perl)
;;(require 'my-coding-php)
(require 'my-coding-python)
;; (require 'my-coding-scheme)

(require 'my-desktop)

(require 'init-google-translate)

;; My code
(require 'sk-lisp)
(require 'sk-idom-article-length)

;; Various packages
(setq mentor-rtorrent-external-rpc "scgi://127.0.0.1:5000")
(defun my-mentor ()
  (interactive)
  (when (require 'mentor nil t)
    (setq mentor-highlight-enable t)
    (setq mentor-view-columns
          '(((mentor-torrent-get-state) -3 "State")
            ((mentor-torrent-get-progress) -3 "Cmp")
            (name -40 "Name")
            ((mentor-torrent-get-speed-up) -6 "Up")
            ((mentor-torrent-get-speed-down) -6 "Down")
            ((mentor-torrent-get-size) -15 "     Size")
            (message -40 "Message")
            (directory -100 "Directory")
            (tied_to_file -80 "Tied file name")))))

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

(when (>= emacs-major-version 24)
  (require 'zenburn-theme))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/predictive"))

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

;; Settings for MacOS
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)
(setq ns-right-alternate-modifier 'none)             ; use right alt for special characters

;; Fix path for MacOSX
(when (and (memq window-system '(mac ns))
           (fboundp 'exec-path-from-shell-initialize))
  (exec-path-from-shell-initialize))

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
            (insert (concat ";; " (substring (emacs-version) 0 16) "."))
            (newline-and-indent)  (newline-and-indent)))

;; Echo .emacs load time
(when (not noninteractive)
  (message ".emacs loaded in %ds"
           (destructuring-bind (hi lo ms ps) (current-time)
             (- (+ hi lo) (+ (first *emacs-start-time*)
                             (second *emacs-start-time*))))))
