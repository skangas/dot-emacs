(defun sk-replace-in-literal-string (regexp to-string)
  "Search and replace only in literal string"
  (interactive
   (let* ((from (read-from-minibuffer "Query replace inside literal string " " "))
          (to   (read-from-minibuffer (format "Query replace inside literal string from %s to " from) "_")))
     (list from to)))
  (re-search-forward "\".+?\"")
  (save-excursion
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      (beginning-of-buffer)
      (ignore-errors
        (while (re-search-forward regexp)
          (replace-match to-string)))))
  (re-search-forward regexp))
