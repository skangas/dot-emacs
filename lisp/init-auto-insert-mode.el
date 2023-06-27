;;; init-auto-insert-mode.el

(auto-insert-mode t)
(setq auto-insert-directory "~/.emacs.d/templates")
(setq auto-insert-query nil)

(with-eval-after-load 'autoinsert
  ;; Reset to default
  (custom-reevaluate-setting 'auto-insert-alist)
  (define-auto-insert
    '(c-mode . "C Program")
    ["c-template" my-auto-update-source-file])

  ;;;; FIXME: Commented out the Python insertion for now.  I had the problem
  ;;;; that when I jumped to a module with LSP, I landed in an empty __init__.py
  ;;;; file where the auto-insert triggered.  Could I make `auto-insert-mode'
  ;;;; not insert in such files?

  ;; (define-auto-insert
  ;;   '(python-mode . "Python Program")
  ;;   ["python-template" my-auto-update-source-file])

  (define-auto-insert
    '(cperl-mode . "Perl Program")
    ["perl-template" my-auto-update-source-file])
  (define-auto-insert
    '(sh-mode . "Shell Script")
    ["shell-template" my-auto-update-source-file])
  (define-auto-insert
    '(org-mode . "Org")
    '(nil
      "#+TITLE:  " (read-string "Title: ") "
#+DATE:   " (format-time-string "%Y-%m-%d") "
#+AUTHOR: SK
#+STARTUP: content hidestars indent
#+OPTIONS: toc:nil num:1 email:nil

" _))
  (define-auto-insert
    (expand-file-name "~/\\.emacs\\.d/.*\\.el")
    '(nil
      ";;; " (format "%s%76s" (file-name-nondirectory buffer-file-name) " -*- lexical-binding: t; -*-") "

" _ "

\(provide '" (file-name-base (buffer-file-name)) ")\n"))

  ;; (define-auto-insert
  ;;   '(org-mode . "Org-mode File")
  ;;   '["org-mode-template" my-auto-update-source-file])
  )

(defun my-auto-update-source-file ()
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
