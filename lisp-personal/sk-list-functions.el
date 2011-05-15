(defun sk-zip-lists (list1 list2)
  (let ((result '()))
    (while list1
      (setq result (cons `(,(car list1) . ,(car list2)) result))
      (setq list1 (cdr list1))
      (setq list2 (cdr list2)))
    (reverse result)))

(provide 'sk-list-functions)
