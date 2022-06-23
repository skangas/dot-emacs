;;;; init-compat.el


;;; Emacs 29 master branch
(unless (get 'magit--handle-bookmark 'bookmark-handler-type)
  (put 'magit--handle-bookmark 'bookmark-handler-type "Magit"))

(unless (fboundp 'recentf-open) ; New in Emacs 29
  (defun recentf-open (file)
    "Prompt for FILE in `recentf-list' and visit it.
Enable `recentf-mode' if it isn't already."
    (interactive
     (list
      (progn (unless recentf-mode (recentf-mode 1))
             (completing-read (format-prompt "Open recent file" nil)
                              recentf-list nil t))))
    (when file
      (funcall recentf-menu-action file))))


;;;; Disable enriched-mode permanently due to below security issues.
(fmakunbound 'enriched-mode)
(fmakunbound 'enriched-decode)
(fmakunbound 'enriched-encode)


;; Stefan Monnier @ emacs-devel 2021-04-01

;; Makes sense, indeed.  It might be worth having people play with the
;; value for a while (and not just for bulk-bandwidth tests but also for
;; interactive use like `M-x shell`, `M-x compile`, `M-x grep`, including
;; with largish outputs) and report back.
;;
;; I just put (setq read-process-output-max (max 65536 read-process-output-max))
;; into my init file, and I'll see if I notice a difference.
;; [ BTW, I think a value >=32kB would be desirable since it avoids
;;   breaking UDP datagrams.  ]
;;
;; Stefan

;; Maybe faster:
(setq read-process-output-max (max read-process-output-max (* 64 1024)))


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
