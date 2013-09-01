;;
;; ~skangas/.emacs
;;

(defvar *emacs-load-start* (current-time))

(let ((d1 "~/.emacs.d/elpa/auto-compile-20130612.152/")
      (d2 ".emacs.d/elpa/packed-20130502.2340/"))
  (when (and (file-directory-p d1)
	     (file-directory-p d2))
    (add-to-list 'load-path d1)
    (add-to-list 'load-path d2)
    (require 'auto-compile)
    (auto-compile-on-load-mode 1)
    (auto-compile-on-save-mode 1)))

;; Get this over with. Has to be a require.
(require 'cl)

;; various stuff 
(setq message-log-max 1024) ;; do this first
(setq max-specpdl-size 15600)
(setq max-lisp-eval-depth 9000)

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
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

;; Create necessary directories
(dolist (dir '("~/.emacs.d/cache" "~/.emacs.d/cache/semanticdb"))
  (unless (file-directory-p dir)
    (make-directory dir)))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;; Require my configuration files
(require 'my-color-theme)
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
;; (require 'my-w3m)

(require 'my-coding)
(require 'my-cedet)
(require 'my-coding-c)
;; (require 'my-coding-common-lisp)
(require 'my-coding-cpp)
(require 'my-coding-elisp)
;; (require 'my-coding-haskell)
(require 'my-coding-java)
(require 'my-coding-perl)
;;(require 'my-coding-php)
;;(require 'my-coding-python)
(require 'my-coding-scheme)

(require 'my-desktop)

(require 'init-google-translate)

(require 'sk-lisp)

;; Various packages
;; (when (require 'mentor nil t)
;;   (setq mentor-highlight-enable t)
;;   (setq mentor-view-columns
;;     '(((mentor-torrent-get-state) -3 "State")
;;       ((mentor-torrent-get-progress) -3 "Cmp")
;;       (name -40 "Name")
;;       ((mentor-torrent-get-speed-up) -6 "Up")
;;       ((mentor-torrent-get-speed-down) -6 "Down")
;;       ((mentor-torrent-get-size) -15 "     Size")
;;       (message -40 "Message")
;;       (directory -100 "Directory")
;;       (tied_to_file -80 "Tied file name"))))

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/predictive"))

(when (condition-case nil
          (require 'completion-ui)
        (error nil))
  (eval-after-load "cc-mode"
    '(progn
       (define-key c-mode-map [?\M-\t] 'complete-semantic)
       (define-key c++-mode-map [?\M-\t] 'complete-semantic))))

;; don't clutter .emacs with M-x customize stuff
(setq custom-file "~/.emacs.d/my-custom-file.el")
(load custom-file 'noerror)

;; Show current version (this needs to be last to be on top)
(add-hook 'after-init-hook
          (lambda ()
            (insert (concat ";; " (substring (emacs-version) 0 16) "."))
            (newline-and-indent)  (newline-and-indent)))

;; FIXME: Time .emacs load time
;; (message ".emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;;                            (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
