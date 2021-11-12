;;; sk-misc.el --- random functions

;; Copyright (C) 2010-2020 Stefan Kangas

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

(defun sk/ert-run-all-tests ()
  (interactive)
  (require 'ert)
  (require 'ert-x)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert-run-tests-interactively "t"))

(defvar sk/previous-random-line nil
  "Previous random line jumped to by `goto-random-line'.")

(defun goto-random-line ()
  "Go to a random line in this buffer."
  (interactive)
  (random t)
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

(global-set-key [(hyper l)] 'my-fix-command)
(defvar my-prev-mode nil)
(defun my-fix-command ()
  (interactive)
  (let ((mode nil)
        (regexp "(define-derived-mode \\([^ \t\n]+\\)\\|(defun \\([^ \t\n]+-mode\\) ")
        change bindings)
    (if (not (re-search-forward "^ *\\((interactive\\)" nil t))
        (message "No more interactive in this file")
      (recenter nil t)
      (save-match-data
        (save-excursion
          (beginning-of-defun)
          (let ((form (read (current-buffer))))
            (when (and (listp form)
                       (eq (car form) 'defun))
              (setq bindings
                    (shell-command-to-string
                     (format
                      "cd %s../lisp; grep -r --include '*.el' \"define-key.*'%s\""
                      data-directory (cadr form)))))))
        (save-excursion
          (when (or (re-search-backward regexp nil t)
                    (re-search-forward regexp nil t))
            (setq mode (or (match-string 1) (match-string 2)))))
        (setq change (read-string (format "%sChange to: "
                                          (or bindings ""))
                                  (or mode my-prev-mode))))
      (when (cl-plusp (length change))
        (setq mode change)
        (goto-char (match-beginning 1))
        (let ((form (read (current-buffer))))
          (goto-char (match-beginning 1))
          (forward-char 1)
          (if (> (length form) 1)
              (progn
                (forward-sexp 2)
                (insert " " mode))
            (forward-sexp 1)
            (insert " nil " mode))
          (forward-sexp -1))
        (setq my-prev-mode mode)))))

;; Courtesy of Mattias Engdegård
(defun all-strings (rx)
  "List of all strings matching RX.
Mainly covers output from `regexp-opt' as converted by `xr'."
  (pcase-exhaustive rx
    ((pred stringp) (list rx))
    (`(seq) (list ""))
    (`(seq ,rx . ,rxs)
     (let ((strings1 (all-strings rx)))
       (mapcan (lambda (s2) (mapcar (lambda (s1) (concat s1 s2)) strings1))
               (all-strings `(seq . ,rxs)))))
    (`(or . ,rxs) (mapcan #'all-strings rxs))
    (`(any . ,args)
     ;; Only covers string arguments.
     (mapcan (lambda (arg)
               (mapcar #'char-to-string
                       (replace-regexp-in-string ; Expand ranges.
                        (rx anychar ?- anychar)
                        (lambda (range)
                          (apply #'string (number-sequence (aref range 0)
                                                           (aref range 2))))
                        arg t)))
             args))
    (`(opt . ,rxs) (all-strings `(or (seq . ,rxs) "")))
    (`(group . ,rxs) (all-strings `(seq . ,rxs)))))

(defun sk-convert-to-regexp-opt (beg end)
  (interactive "r")
  (let ((regexp (read
                 (buffer-substring-no-properties beg end)))
        (standard-output (current-buffer))
        (print-level nil)
        (print-length nil)
        words)
    (when (string-match (rx string-start
                            "\\<" (* anychar) "\\>"
                            string-end)
                        regexp)
      (setq regexp (string-trim regexp "\\\\<" "\\\\>"))
      (setq words t))
    (save-excursion
      (delete-region beg end)
      (princ "(regexp-opt '")
      (prin1 (sort (all-strings (xr regexp)) #'string<))
      (when words
        (princ " 'words"))
      (princ ")"))))

(provide 'sk-misc)
;; sk-misc.el ends here
