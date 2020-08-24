;;; sk-misc.el --- random functions

;; Copyright (C) 2010, Stefan Kangas

;; Author: Stefan Kangas
;; Keywords: utilities

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code

(require 'cl-lib)

(defvar sk/previous-random-line nil
  "Previous random line jumped to by `goto-random-line'.")

(defun goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (cl-flet ((get-random-line (lambda () (1+ (random (count-lines (point-min) (point-max)))))))
    (goto-char (point-min))
    (let ((lin (get-random-line)))
      (while (eq lin sk/previous-random-line)
        (setq lin (get-random-line)))
      (forward-line lin)
      (message "Jumped to random line %s" lin)
      (setq sk/previous-random-line lin)))
  (back-to-indentation))

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

(defun sk-test-find-dired ()
  (interactive)
  (find-dired "/mnt/usb/seed/other" "-type f \\( ! -iname '*.jpg' ! -iname '*.jpeg' ! -iname '*.gif' ! -iname '*.bmp' ! -iname '*.html' ! -iname '*.png' ! -iname '*.zip' ! -iname '*.db' ! -iname '*.pdf ! -iname '*.nfo' \\)")
  (sk/go/body)
  (goto-random-line))

(provide 'sk-misc)
;; sk-misc.el ends here
