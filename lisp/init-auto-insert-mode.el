;;; init-auto-insert-mode.el

(eval-when-compile
  (require 'auto-insert))

(defun sk/define-auto-insert (condition action &optional after)
  "Delete all auto inserts before calling `define-auto-insert'."
  (setq auto-insert-alist (assoc-delete-all condition auto-insert-alist))
  (define-auto-insert condition action after))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main config

(use-package autoinsert
  :defer 3
  :ensure nil
  :custom
  (auto-insert-directory (locate-user-emacs-file "templates"))
  :config
  (auto-insert-mode t)

  ;; Reset to default
  (custom-reevaluate-setting 'auto-insert-alist)

  ;; C Program
  (sk/define-auto-insert
   '(c-mode . "C Program")
   ["c-template" sk/auto-update-source-file])

;;;; FIXME: Commented out the Python insertion for now.  I had the problem
;;;; that when I jumped to a module with LSP, I landed in an empty __init__.py
;;;; file where the auto-insert triggered.  Could I make `auto-insert-mode'
;;;; not insert in such files?

  ;; (define-auto-insert
  ;;   '(python-mode . "Python Program")
  ;;   ["python-template" sk/auto-update-source-file])

  ;; Perl
  (sk/define-auto-insert
   '(cperl-mode . "Perl Program")
   ["perl-template" sk/auto-update-source-file])

  ;; Shell
  (sk/define-auto-insert
   '(sh-mode . "Shell Script")
   ["shell-template" sk/auto-update-source-file])

  ;; Org-mode
  (define-skeleton sk/skel-org-mode
    "My org-mode skeleton."
    nil
    "#+TITLE:  " (read-string "Title: ") "
#+DATE:   " (format-time-string "%Y-%m-%d") "
#+AUTHOR: SK
#+STARTUP: content hidestars indent
#+OPTIONS: toc:nil num:1 email:nil

" _)
  (sk/define-auto-insert
   '(org-mode . "Org")
   ;; Don't insert the skeleton in my org-roam or journal files.
   (lambda ()
     (when (not (string-match (rx bos (eval (expand-file-name "~/org/"))
                                  (or "roam" "journal") "/")
                              buffer-file-name))
       (sk/skel-org-mode))))

  ;; Emacs Lisp
  (sk/define-auto-insert
   (expand-file-name "~/\\.emacs\\.d/.*\\.el")
   '(nil
     ";;; " (format "%s%76s" (file-name-nondirectory buffer-file-name) " -*- lexical-binding: t; -*-") "

" _ "

\(provide '" (file-name-base (buffer-file-name)) ")\n"))

  ;; (define-auto-insert
  ;;   '(org-mode . "Org-mode File")
  ;;   '["org-mode-template" sk/auto-update-source-file])
  )

(defun sk/auto-update-source-file ()
  ;; replace HEADER_NAME with something suitable for an ifdef
  (save-excursion
    (while (search-forward "HEADER_NAME" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match
         (replace-regexp-in-string "[-.]" "_" (upcase (file-name-nondirectory buffer-file-name)))))))
  ;; Replace @@@ with file name
  (save-excursion
    (while (search-forward "@@@" nil t)
      (save-restriction
	(narrow-to-region (match-beginning 0) (match-end 0))
	(replace-match (file-name-nondirectory buffer-file-name)))))
  ;; replace YYYY with current year
  (save-excursion
    (while (search-forward "YYYY" nil t)
      (save-restriction
	(narrow-to-region (match-beginning 0) (match-end 0))
	(replace-match (format-time-string "%Y" (current-time)))))))

(provide 'init-auto-insert-mode)
