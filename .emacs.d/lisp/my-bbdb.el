(require 'bbdb-loaddefs nil t)
(require 'bbdb nil t)

(eval-after-load 'bbdb
  (when (> (string-to-number bbdb-version) 3)
    (when (fboundp 'bbdb-initialize)
      (bbdb-initialize 'gnus 'message))

    (setq bbdb-file "~/.emacs.d/bbdb")

    (setq bbdb-mail-user-agent 'gnus ;; Use gnus for sending email
          ;; bbdb-electric-p t          ;; be disposable with SPC
          ;; bbdb-pop-up-target-lines  1 ;; very small (??? do I want this)
          ;; bbdb-allow-name-mismatches t ;; do not show name-mismatches (??? do I want this)

          ;; add new addresses to existing contacts automatically
          bbdb-add-mails 'query

          ;; cycle through matches (this only works partially (??? still correct?))
          bbdb-complete-mail-allow-cycling t 

          bbdb-use-alternate-names t ;; use AKA

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
                                             "kangass@student.chalmers.se")))))

(provide 'my-bbdb)

;; my-bbdb.el ends here
