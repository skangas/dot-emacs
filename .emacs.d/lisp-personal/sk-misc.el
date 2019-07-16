(defun goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (goto-char (point-min))
  (forward-line (1+ (random (count-lines (point-min) (point-max))))))

;; (defun my-convert-iso8859-1-to-utf-8 ()
;;   (interactive)
;;   (save-excursion
;;     (dolist (char '(("Ã¥" "å")
;;                     ("Ã¤" "ä")
;;                     ("ã¶" "ö")))
;;       (beginning-of-buffer
;;        (while (search-forward (car char) nil t)
;;          (replace-match (cadr char) t nil))))))

;; (defun my-sgml-delete-tag-and-everything ()
;;   "delete text between the tags that contain the current point"
;;   (interactive)
;;   (let ((b (point)))
;;     (sgml-skip-tag-backward 1)
;;     (when (not (eq b (point)))
;;       ;; moved somewhere, should be at front of a tag now
;;       (save-excursion
;;         (setq b (point)))
;;       (concat "</?[^>]*>\n?")
;;       (forward-sexp 1)
;;       (sgml-skip-tag-forward 1)
;; ;      (backward-sexp 1)
;;       (delete-region b (point)))))

;; (defmacro my-run-on-html-tags (fun tags)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (let ((re (concat "</?" (regexp-opt tags) "[^>]*>\n?")))
;;       (while (search-forward-regexp re nil t)
;;         (funcall fun)))))

;; (defun my-delete-html-tags (tags)
;;   (my-run-on-html-tags tags (lambda ()
;;                               (sgml-delete-tag))))

;; (defun my-delete-html-tags-in-org-export-for-publication ()
;;   (interactive)
;;   (my-delete-html-tags '("\?xml" "!DOCTYPE" "html" "head" "title" "meta" "style" "script" "\!--" "!\[CDATA" "body" "div")))

(provide 'sk-misc)
;; sk-misc.el ends here
