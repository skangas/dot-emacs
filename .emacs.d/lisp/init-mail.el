(progn
  (setq message-user-fqdn "marxist.se") ; used to generate Message-ID
  (setq message-fill-column 70)

  (pcase system-type
    ('gnu/linux
     (setq message-send-mail-function 'message-send-mail-with-sendmail)
     (setq sendmail-program "/usr/bin/msmtp"))
    ('darwin
     (load-file "~/org/misc/.osx-sendmail.el")))

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

(with-eval-after-load 'notmuch

  ;; KEYBINDINGS
  (defun sk-notmuch-delete-message ()
    "toggle deleted tag for message"
    (interactive)
    (if (member "deleted" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-deleted"))
      (notmuch-show-tag (list "+deleted"))))

  (define-key notmuch-show-mode-map "d" #'sk-notmuch-delete-message)

  ())

(provide 'init-mail)
