;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoInsertMode

(add-hook 'find-file-hook auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates/") ; Keep trailing slash
;; (setq auto-insert-query nil) ; Do not prompt before insertion

;; (add-hook 'cperl-mode-hook 'auto-insert)

(eval-after-load 'autoinsert
  (define-auto-insert
    '(cperl-mode . "Perl Program")
    '["perl-template" my-auto-update-source-file])
  (define-auto-insert
    '(sh-mode . "Shell Script")
    '["shell-template" my-auto-update-source-file])
  (define-auto-insert
    '(org-mode . "Org-mode File")
    '["org-mode-template" my-auto-update-source-file]))

;; (setq auto-insert-alist
;;       '((("\\.h\\(pp\\)?$" "C/C++ Header") . ["c-header-template" my-auto-update-source-file])
;;

;; ))

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
