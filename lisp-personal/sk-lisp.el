
(defun sk-search-and-replace (replace-list)
  (save-excursion
    (dolist (replace replace-list)
      (goto-char 0)
      (while (re-search-forward (car replace) nil t)
        (replace-match (cadr replace) t)))))

(defun sk-clean-html-entities ()
  (interactive)
  (let ((entities '(("&quot;" "\"")
                    ("&hellip;" "...")
                    ("&ldquo;" "\"")
                    ("&rdquo;" "\"")
                    ("&rsquo;" "\"")
                    ("&shy;" "")
                    ("&eacute;" "é")
                    ("&aring;" "å")
                    ("&Aring;" "A")
                    ("&auml;" "ä")
                    ("&Auml;" "Ä")
                    ("&ouml;" "ö")
                    ("&#148;" "\"")
                    ("&#160;" "&nbsp;")
                    ("&Ouml;" "Ö"))))
    (sk-search-and-replace entities)))

(defun sk-cleanerrrr ()
  (interactive)
  (let ((crap '(("" "")
                ("Ã¥" "å")
                ("Ã¤" "ä")
                ("Ã¶" "ö")
                )))
    (sk-search-and-replace crap))
  (sk-clean-html-entities)
  (delete-trailing-whitespace))

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

(defun sk-fix-org-html-export-for-web ()
  (interactive)
  (save-excursion
    ;; Remove part before content
    (goto-char (point-min))
    (re-search-forward "^<div id=\"content\">$")
    (beginning-of-line)
    (next-logical-line)
    (delete-region (point) (point-min))
    (goto-char (point-min))
    
    (while (re-search-forward "</?div[^>]*>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "</?h1[^>]*>" nil t)
      (replace-match ""))
    ;; h2 -> h3
    (goto-char (point-min))
    (while (re-search-forward "\\(</?h\\)2[^>]*>" nil t)
      (replace-match "\\13>"))
    ;; i -> em
    (goto-char (point-min))
    (while (re-search-forward "\\(</?\\)i[^>]*>" nil t)
      (replace-match "\\1em>"))
    ;; b -> strong
    (goto-char (point-min))
    (while (re-search-forward "\\(</?\\)b>" nil t)
      (replace-match "\\1strong>"))
    (goto-char (point-min))
    ;; remove class
    (goto-char (point-min))
    (while (re-search-forward " class=\"(footref\\|footnum\\|org-ol)\"" nil t)
      (replace-match ""))
    (goto-char (point-min))
    ;; Remove part after content
    (re-search-forward "^<p class=\"date\">")
    (beginning-of-line)
    (delete-region (point) (point-max))))

(defun sk-fix-indesign-html-export-for-web ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^		<div id=\"")
    (beginning-of-line)
    (next-logical-line)
    (delete-region (point) (point-min))
    (goto-char (point-min))

    ;; ITALICS
    (while (re-search-forward "<span class=\"Character-Style-1\">" nil t)
      (replace-match "<em>")
      (re-search-forward "</span>")
      (replace-match "</em>"))
    (while (re-search-forward "<span class=\"Italic[^\"]*\">" nil t)
      (replace-match "<em>")
      (re-search-forward "</span>")
      (replace-match "</em>"))

    ;; Old style digits
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"Old-style-digits[^\"]*\">" nil t)
      (replace-match "")
      (re-search-forward "</span>")
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"Small-caps[^\"]*\">" nil t)
      (replace-match "")
      (re-search-forward "</span>")
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"CharOverride-[0-9]\">" nil t)
      (replace-match "")
      (re-search-forward "</span>")
      (replace-match ""))

    ;; Fix headers
    (goto-char (point-min))
    (while (re-search-forward "<p class=\"Rubrik-2\">" nil t)
      (replace-match "<h3>")
      (re-search-forward "</p>")
      (replace-match "</h3>"))
    (while (re-search-forward "<p class=\"Rubrik-3\">" nil t)
      (replace-match "<h3>")
      (re-search-forward "</p>")
      (replace-match "</h3>"))
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"Kapitelnamn\">" nil t)
      (replace-match "<h1>")
      (re-search-forward "</span>")
      (replace-match "</h1>"))

    ;; Fix footnotes
    (goto-char (point-min))
    (while (re-search-forward " href=\"[^#]+\.html#" nil t)
      (replace-match " href=\"#"))
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"endnote_marker[^\"]*\">" nil t)
      (replace-match "<sup>[")
      (re-search-forward "</span>")
      (replace-match "]</sup>"))

    ;; Remove any remaining classes
    (sk-search-and-replace
     '((" class=\"Rubrik-1[^\"]*\"" "")
       (" class=\"Författare[^\"]*\"" "")
       (" class=\"Brödtext-First-Paragraph[^\"]*\"" "")
       (" class=\"Brödtext[^\"]*\"" "")
       (" class=\"Fotnot-numbered[^\"]*\"" "")
       (" class=\"Rubrik-2[^\"]*\"" "")
       ;;; OLD
       (" class=\"Body_BF-Body-First-Paragraph[^\"]*\"" "")
       (" class=\"Body_BT-BodyText[^\"]*\"" "")
       (" class=\"Body_BF-Body-First-Paragraph[^\"]*\"" "")
       ))))

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

(provide 'sk-lisp)
