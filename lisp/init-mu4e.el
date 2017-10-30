(add-to-list 'load-path (expand-file-name "~/usr/share/emacs/site-lisp/mu4e"))

(require 'mu4e)

(setq
 mu4e-maildir       "~/Maildir"     ;; top-level Maildir
 mu4e-sent-folder   "/.Sent"        ;; folder for sent messages
 mu4e-drafts-folder "/.Drafts"      ;; unfinished messages
 mu4e-trash-folder  "/.Papperskorg" ;; trashed messages
 mu4e-refile-folder "/archive"      ;; saved messages

 mu4e-user-mail-address-list '("skangas@skangas.se" "stefankangas@gmail.com")

 mu4e-reply-to-address "skangas@skangas.se"
 user-mail-address "skangas@skangas.se"
 user-full-name "Stefan Kangas"

 mu4e-view-show-images t
 mu4e-view-image-max-width 800)

(provide 'init-mu4e)
