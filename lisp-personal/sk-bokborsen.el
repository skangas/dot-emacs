;;; sk-bokborsen.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun sk/bokborsen-to-ledger ()
  "Convert from BB to Ledger."
  (interactive)
  (goto-char (pos-bol))
  (while (and (not (= (point) (point-max)))
              (looking-at (rx bol (= 16 (any digit "-: ")) "\C-i")))
    (sk/bokborsen-transform-line)
    (forward-line 1)))

(defun sk/bokborsen-transform-line ()
  (save-restriction
    (narrow-to-region (pos-bol) (pos-eol))
    (re-search-forward (rx (group (= 4 digit)) "-"
                           (group (= 2 digit)) "-"
                           (group (= 2 digit))
                           " " (= 2 digit) ":" (= 2 digit)
                           "\C-i"))
    (replace-match "\\1/\\2/\\3 ")
    (cond
     ((looking-at "Frakt")
      (insert "Bokbörsen ")
      (search-forward "\C-i")
      (delete-char -1)
      (insert "
    Expense:Frakt:Bokbörsen                    62,00 kr
    Assets:Bokbörsen säljarsaldo              -62,00 kr
")
      (delete-region (point) (pos-eol)))
     ((looking-at "Inbetalning")
      (insert "Bokbörsen ")
      (search-forward "\C-i")
      (delete-char -1)
      (insert "
    Assets:Kassa                                -100 kr
    Assets:Bokbörsen säljarsaldo                 100 kr
")
      (delete-region (point) (pos-eol)))
     ((looking-at "Orderavgift")
      (insert "Bokbörsen ")
      (search-forward "\C-i")
      (delete-char -1)                  ; delete TAB
      (delete-char 1)                   ; delete "-"
      (looking-at (rx (group (one-or-more (any "," digit)))))
      (delete-region (point) (pos-eol))
      (insert "
    Expense:Bokbörsen:Avgift                   %s kr
    Assets:Bokbörsen säljarsaldo              -%s kr
" )
      (match-string 1)

      ))))

(provide 'sk-bokborsen)

;;; sk-bokborsen.el ends here
