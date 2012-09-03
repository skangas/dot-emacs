(require 'bbdb-loaddefs)
(require 'bbdb)

;; initial configuration
(when (fboundp 'bbdb-initialize)
  (bbdb-initialize 'gnus 'message))

(eval-after-load "bbdb"
  '(progn
     (setq bbdb-file "~/.emacs.d/bbdb")

     (if (> (string-to-number bbdb-version) 3)
         ;; BBDB 3.x
         (progn
           (setq bbdb-mail-user-agent 'gnus ;; Use gnus for sending email
                 ;; bbdb-electric-p t          ;; be disposable with SPC
                 ;; bbdb-pop-up-target-lines  1 ;; very small (??? do I want this)
                 ;; bbdb-allow-name-mismatches t ;; do not show name-mismatches (??? do I want this)

                 ;; add new addresses to existing contacts automatically
                 bbdb-add-mails 'query

                 ;; cycle through matches (this only works partially (??? still correct?))
                 bbdb-complete-mail-allow-cycling t 

                 bbdb-use-alternate-names t     ;; use AKA

                 ;;bbdb-elided-display t                    ;; single-line addresses

                 bbdb-default-area-code "070"
                 bbdb-phone-style nil
                 bbdb-check-postcode nil
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
                                                    "kangass@student.chalmers.se"))))
       ;; BBDB 2.x
       (progn
         ;; (eval-after-load "gnus"
         ;;   (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
         ;; (bbdb-insinuate-w3)

         (setq bbdb-send-mail-style 'gnus ;; Use gnus for sending email

               bbdb-offer-save 1 ;; 1 is save without asking

               bbdb-use-pop-up t          ;; allow popups for addresses
               bbdb-electric-p t          ;; be disposable with SPC
               bbdb-popup-target-lines  1 ;; very small

               bbdb-dwim-net-address-allow-redundancy t ;; always use full name
               bbdb-quiet-about-name-mismatches t ;; do not show name-mismatches

               bbdb-always-add-address t ;; add new addresses to existing...
               ;; ...contacts automatically
               bbdb-canonicalize-redundant-nets-p t ;; x@foo.bar.cx => x@bar.cx

               bbdb-completion-type nil ;; complete on anything

               bbdb-complete-name-allow-cycling t ;; cycle through matches
               ;; this only works partially

               bbbd-message-caching-enabled t ;; be fast
               bbdb-use-alternate-names t     ;; use AKA

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

         ;; Auto-create outgoing
         (setq bbdb/send-auto-create-p 'prompt)
         (setq bbdb/send-prompt-for-create-p t)

         ;; from the FAQ -- fixes needing to restart emacs after adding aliases
         (add-hook 'message-setup-hook 'bbdb-define-all-aliases)))))

(provide 'my-bbdb)

;; my-bbdb.el ends here
