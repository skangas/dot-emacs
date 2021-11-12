;;;; init-compat.el


;;;; Disable enriched-mode permanently due to below security issues.
(fmakunbound 'enriched-mode)
(fmakunbound 'enriched-decode)
(fmakunbound 'enriched-encode)


;;;; Emacs < 27.1

;; Increase min bits to 2048 for old Emacs
;; https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00718.html
(when (and (< emacs-major-version 27)
           (boundp 'gnutls-min-prime-bits))
  (setq gnutls-min-prime-bits (max 2048 gnutls-min-prime-bits)))


;;;; Emacs < 25.3

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


;;;; Emacs < 25.1

(when (< emacs-major-version 25)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(provide 'init-compat)
