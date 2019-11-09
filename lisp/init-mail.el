(progn
  (setq message-user-fqdn "marxist.se") ; used to generate Message-ID
  (setq message-fill-column 70)

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp")

  ;; Generate the mail headers before you edit your message.
  (setq message-generate-headers-first t)

  ;; Specify the envelope-from address when sending mail.
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)

  ;; Addresses to prune when doing wide replies.
  (setq message-dont-reply-to-names (seq-uniq
                                     `("stefankangas@gmail.com"
                                       "stefan@marxist.se"
                                       "skangas@skangas.se"
                                       "skangas@fripost.org"
                                       ,user-mail-address
                                       ;; Never reply here.
                                       "control@debbugs.gnu.org"
                                       "bug-gnu-emacs@gnu.org"
                                       "emacs-pretest-bug@gnu.org"
                                       )))



  )

(provide 'init-mail)
