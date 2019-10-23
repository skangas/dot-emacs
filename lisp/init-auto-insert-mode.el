;;; init-auto-insert-mode.el

(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates/") ; Keep trailing slash
(setq auto-insert-query nil)

(string-match "~/org/.*\\.org\\'"
              "~/org/foo.org")

(eval-after-load 'autoinsert
  ;; Reset to default
  '(progn
     (custom-reevaluate-setting 'auto-insert-alist)

     ;; Perl
     (define-auto-insert
       '(cperl-mode . "Perl Program")
       '["perl-template" my-auto-update-source-file])

     ;; Shell
     (define-auto-insert
       '(sh-mode . "Shell Script")
       '["shell-template" my-auto-update-source-file])

     ;; Emacs Configuration
     (define-auto-insert
       (expand-file-name "~/\\.emacs\\.d/.*\\.el")
       '(nil
         ";;; " (format "%s%76s" (file-name-nondirectory buffer-file-name) " -*- lexical-binding: t; -*-") "

" _ "

\(provide '" (file-name-base (buffer-file-name)) ")\n"))

     ;; *.org
     (define-auto-insert
       '(org-mode . "Org")
       '(nil
         "#+TITLE:  " (read-string "Title: ") "
#+DATE:   " (format-time-string "%Y-%m-%d") "
#+AUTHOR: SK
#+STARTUP: content hidestars indent
#+OPTIONS: toc:nil num:1 email:nil

" _)))

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
