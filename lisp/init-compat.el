;; Increase min bits to 2048 for old Emacs
(when (and (boundp 'gnutls-min-prime-bits)
           gnutls-min-prime-bits)
  (setq gnutls-min-prime-bits (max 2048 gnutls-min-prime-bits)))

;; From etc/NEWS.25:
;;
;; *** Enriched Text mode has its support for decoding 'x-display' disabled.
;; This feature allows saving 'display' properties as part of text.
;; Emacs 'display' properties support evaluation of arbitrary Lisp forms
;; as part of instantiating the property, so decoding 'x-display' is
;; vulnerable to executing arbitrary malicious Lisp code included in the
;; text (e.g., sent as part of an email message).
;;
;; This vulnerability was introduced in Emacs 21.1.  To work around that
;; in Emacs versions before 25.3, append the following to your ~/.emacs
;; init file:
(if (and (< emacs-major-version 26)
         (not (and (= emacs-major-version 25)
                   (= emacs-minor-version 3))))
    (eval-after-load "enriched"
      '(defun enriched-decode-display-prop (start end &optional param)
         (list start end))))

(provide 'init-compat)
;; init-compat.el ends here
