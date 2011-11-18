;; initial configuration
(when (fboundp 'bbdb-initialize)
  (bbdb-initialize 'gnus 'message))
(eval-after-load "gnus"
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
;; (bbdb-insinuate-w3)

;; various settings
(setq bbdb-file "~/.emacs.d/bbdb")

(eval-after-load "bbdb"
  '(progn
     (setq bbdb-offer-save 1                        ;; 1 is save without asking

           bbdb-use-pop-up t                        ;; allow popups for addresses
           bbdb-electric-p t                        ;; be disposable with SPC
           bbdb-popup-target-lines  1               ;; very small

           bbdb-dwim-net-address-allow-redundancy t ;; always use full name
           bbdb-quiet-about-name-mismatches t       ;; do not show name-mismatches

           bbdb-always-add-address t                ;; add new addresses to existing...
                                                    ;; ...contacts automatically
           bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

           bbdb-completion-type nil                 ;; complete on anything

           bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                                    ;; this only works partially

           bbbd-message-caching-enabled t           ;; be fast
           bbdb-use-alternate-names t               ;; use AKA

           ;;bbdb-elided-display t                    ;; single-line addresses

           bbdb-default-area-code "070"
           bbdb-north-american-phone-numbers-p nil
           bbdb-check-zip-codes-p nil
           bbbdb-default-country "Sweden"
           bbdb-user-mail-names (regexp-opt '("skangas@skangas.se"
                                              "stefan@fripost.org"
                                              "skangas@fripost.org"
                                              "skangas@fsfe.org"
                                              "stefan@marxist.se"
                                              "stefankangas@gmail.com"
                                              "skangas@cpan.org"
                                              "stekan01@student.hgo.se"
                                              "stefan.kangas@vansterpartiet.se"
                                              "stefan.kangas@ungvanster.se"
                                              "kangass@student.chalmers.se")))

     ;; from the FAQ -- fixes needing to restart emacs after adding aliases
     (add-hook 'message-setup-hook 'bbdb-define-all-aliases)))

     ;; Auto-creation of all messages addressed to me (AutoCreateBbdbEntries)
     ;; (setq bbdb/mail-auto-create-p 'bbdb-prune-not-to-me)
     ;; (setq bbdb/news-auto-create-p 'bbdb-prune-not-to-me)
     ;; (defun bbdb-prune-not-to-me () nil)
;;        "defun called when bbdb is trying to automatically create a record.  Filters out
;; anything not actually adressed to me then passes control to 'bbdb-ignore-some-messages-hook'.
;; Also filters out anything that is precedense 'junk' or 'bulk'  This code is from
;; Ronan Waide < waider @ waider . ie >."
;;        (let ((case-fold-search t)
;;              (done nil)
;;              (b (current-buffer))
;;              (marker (bbdb-header-start))
;;              field regexp fieldval)
;;          (set-buffer (marker-buffer marker))
;;          (save-excursion
;;            ;; Hey ho. The buffer we're in is the mail file, narrowed to the
;;            ;; current message.
;;            (let (to cc precedence)
;;              (goto-char marker)
;;              (setq to (bbdb-extract-field-value "To"))
;;              (goto-char marker)
;;              (setq cc (bbdb-extract-field-value "Cc"))
;;              (goto-char marker)
;;              (setq precedence (bbdb-extract-field-value "Precedence"))
;;              ;; Here's where you put your email information.
;;              ;; Basically, you just add all the regexps you want for
;;              ;; both the 'to' field and the 'cc' field.
;;              (if (and (not (string-match "stefan@marxist.se" (or cc "")))
;;                       (not (string-match "stefankangas@gmail.com" (or to "")))
;;                       (not (string-match "skangas@fsfe.org" (or cc "")))
;;                       (not (string-match "skangas@cpan.org" (or cc ""))))
                 
;;                  (progn
;;                    (message "BBDB unfiling; message to: %s cc: %s"
;;                             (or to "noone") (or cc "noone"))
;;                    ;; Return nil so that the record isn't added.
;;                    nil)

;;                (if (string-match "junk" (or precedence ""))
;;                    (progn
;;                      (message "precedence set to junk, bbdb ignoring.")
;;                      nil)

;;                  ;; Otherwise add, subject to filtering
;;                  (bbdb-ignore-some-messages-hook)))))))))


(provide 'my-bbdb)

;; my-bbdb.el ends here
