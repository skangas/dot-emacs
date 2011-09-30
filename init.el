;;
;; ~skangas/.emacs
;;

;; Let's put this here, in case we need it. Has to be a require.
(require 'cl)
(defvar *emacs-load-start* (current-time))

;; Enter the debugger when an error is found
(setq debug-on-error t)

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/jdee-2.4.0.1/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/magit/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal/mentor/"))
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/skangas-detached-worktree/"))

(require 'warnings) ;; work-around until Emacs > 23.2 is released

(require 'sunrise-commander)

;; Require my configuration files
(require 'my-color-theme)

(require 'my-general)
(require 'my-emacs-server)
(require 'my-keybindings)

(require 'my-abbrev)
(require 'my-auto-insert-mode)
(require 'my-bbdb)
(require 'my-buffers)
(require 'my-dired)
(require 'my-ediff)
(require 'my-emms)
(require 'my-org-mode)
(require 'my-outline)
(require 'my-rcirc)
(require 'my-tramp)
(require 'my-w3m)

(require 'my-coding)
(require 'my-cedet)
(require 'my-coding-c)
(require 'my-coding-cpp)
(require 'my-coding-common-lisp)
(require 'my-coding-elisp)
;; (require 'my-coding-haskell)
(require 'my-coding-html-css)
(require 'my-coding-java)
(require 'my-coding-perl)
(require 'my-coding-scheme)
;; (require 'my-coding-php)

(require 'my-desktop)

(require 'my-z-end)

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

;; Time .emacs load time
(message "My .emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                           (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))

;; only for certain modes
(setq sentence-end "\\.  ?")
