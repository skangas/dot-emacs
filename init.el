;;
;; ~skangas/.emacs
;;

(defvar *emacs-load-start* (current-time))

;; Get this over with. Has to be a require.
(require 'cl)

;; Do not enter the debugger when an error is found...
(setq debug-on-error nil)

;; Add local elisp directories
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/jdee-2.4.0.1/lisp/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/magit/"))
(add-to-list 'load-path (expand-file-name "~/wip/mentor"))

(let ((byte-compiled "~/.emacs.d/lisp/geiser/build/elisp/geiser-load")
      (in-place "~/.emacs.d/lisp/geiser/elisp/geiser.el"))
  (if (file-exists-p byte-compiled)
     (load byte-compiled)
   (load-file in-place)))

(require 'warnings) ;; work-around until Emacs > 23.2 is released

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
;; (require 'my-emms)
(require 'my-org-mode)
(require 'my-outline)
(require 'my-rcirc)
(require 'my-tramp)
(require 'my-w3m)

(require 'my-coding)
(require 'my-cedet)
(require 'my-coding-c)
;; (require 'my-coding-common-lisp)
(require 'my-coding-cpp)
(require 'my-coding-elisp)
;; (require 'my-coding-haskell)
(require 'my-coding-html-css)
(require 'my-coding-java)
(require 'my-coding-perl)
(require 'my-coding-php)
(require 'my-coding-scheme)

(require 'my-desktop)

;; Various packages
(autoload 'boxquote "boxquote" "boxquote" t)
(autoload 'mentor "mentor" "mentor" t)
(autoload 'sunrise "sunrise-commander" "sunrise-commander" t)

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
