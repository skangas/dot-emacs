;;; unfill.el --- The inverse of fill-paragraph and fill-region

;; Copyright (C) 2012 Steve Purcell.

;; Author: Steve Purcell <steve@sanityinc.com>
;; Version: 20120529.1250
;; X-Original-Version: DEV
;; Keywords: utilities

;; Based on Xah Lee's examples: http://xahlee.org/emacs/emacs_unfill-paragraph.html

;; This file is NOT part of GNU Emacs.

;;;###autoload
(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

;;;###autoload
(defun unfill-region (start end)
  "Replace newline chars in region from START to END by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(provide 'unfill)
;;; unfill.el ends here
