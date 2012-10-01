(defun sk-cc-file-co ()
  (interactive)
  (shell-command 
   (format "ct co -nc %s" 
           (shell-quote-argument (buffer-file-name))))
  (revert-buffer nil t nil))

(defun sk-cc-file-ci ()
  (interactive)
  (shell-command 
   (format "ct ci -nc %s" 
           (shell-quote-argument (buffer-file-name))))
  (revert-buffer nil t nil))

(defun sk-word-wrapped-to-longlines
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([^[:space:]]\\)\n\\([^[:space:]]\\)")
      (replace-match "\\1 \\1" nil))))

(defun sk-fix-org-html-export-for-web ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^<div id=\"content\">$")
    (beginning-of-line)
    (next-logical-line)
    (delete-region (point) (point-min))
    (while (re-search-forward "</?div[^>]*>" nil t)
      (replace-match ""))
    (while (re-search-forward "\\(</?\\)h2[^>]*>" nil t)
      (replace-match "\\1h3>"))
    (re-search-forward "^<p class=\"date\">")
    (beginning-of-line)
    (delete-region (point) (point-max))))

(defun sk-clean-html ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((ignored-tags (regexp-opt '("div" "span")))
            (clean-tags (regexp-opt '("p" "strong" "b" "em" "i" "h1" "h2" "h3"))))
        (goto-char (point-min))
        (while (re-search-forward (concat "</?" ignored-tags "[^>]*>") nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward (concat "<\\(/?" clean-tags "\\)[^>]*>") nil t)
          (replace-match "<\\1>"))))))

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

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((lines) (end (copy-marker end)))
      (goto-char start)
      (while (and (< (point) (marker-position end))
                  (not (eobp)))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (member line lines)
              (delete-region (point) (progn (forward-line 1) (point)))
            (push line lines)
            (forward-line 1)))))))

;; count words function
(defun count-words (start end)
  "Print number of words in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (message (format "%d" (count-matches "\\sw+"))))))

(provide 'sk-lisp)
