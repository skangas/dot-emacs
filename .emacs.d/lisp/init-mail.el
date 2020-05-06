(progn
  (setq message-user-fqdn "stefankangas.se") ; used to generate Message-ID
  (setq message-fill-column 70)

  ;; (pcase system-type
  ;;   ('gnu/linux
  ;;    (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;;    (setq sendmail-program "/usr/bin/msmtp"))
  ;;   ('darwin
  ;;    (load-file "~/org/misc/.osx-sendmail.el")))
  (setq sendmail-program (expand-file-name "~/src/lieer/gmi"))
  (setq message-sendmail-extra-arguments `("--quiet" "send" "-C" "~/.mail/account.gmail"))
  ;; (setq message-sendmail-extra-arguments `("send" "-C" "~/.mail/account.gmail"))
  (setq message-sendmail-f-is-evil t) ;; maybe not needed in latest lieer?
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
                                       "emacs-pretest-bug@gnu.org"))))

(require 'notmuch)
(with-eval-after-load 'notmuch
  (setq notmuch-show-logo nil)
  (setq notmuch-show-all-tags-list t)
  (setq notmuch-search-oldest-first nil)

  ;; Delete message.
  (defun sk/notmuch-show-delete-message ()
    "toggle deleted tag for message in notmuch show mode."
    (interactive)
    (if (member "trash" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-trash"))
      (notmuch-show-tag (list "+trash"))))
  (defun sk/notmuch-search-delete-message ()
    "toggle deleted tag for message in notmuch show mode."
    (interactive)
    (if (member "trash" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-trash"))
      (notmuch-search-tag (list "+trash")))
    (notmuch-search-next-thread))
  (define-key notmuch-search-mode-map "d" 'sk/notmuch-search-delete-message)
  (define-key notmuch-show-mode-map "d" 'sk/notmuch-show-delete-message))


;;;;;;;;;; Handle word attachments.
;; Below code from https://notmuchmail.org/pipermail/notmuch/2017/023855.html

(defun mm-inline-msword (handle)
  (let (text)
    (with-temp-buffer
      (mm-insert-part handle)
      (call-process-region (point-min) (point-max) "antiword" t t nil "-")
      (setq text (buffer-string)))
    (mm-insert-inline handle text)))

(defun mm-inline-docx (handle)
  "pandoc --normalize -r docx -w markdown %s"
  (let (text)
    (with-temp-buffer
      (mm-insert-part handle)
      (let ((coding-system-for-read 'utf-8))
	(call-process-region (point-min) (point-max) (expand-file-name "~/bin/antiwordx") t t nil))
      (setq text (buffer-string)))
    (mm-insert-inline handle text)))

(setq my-inline-mime-tests
     '(("text/rtf" mm-inline-rtf
        (lambda
          (handle)
          (let
              ((name
                (mail-content-type-get
                 (mm-handle-disposition handle)
                 'filename)))
            (and name
                 (equal ".rtf"
                        (substring name -4 nil))))))
       ("application/x-msword" mm-inline-docx
        (lambda
          (handle)
          (let
              ((name
                (mail-content-type-get
                 (mm-handle-disposition handle)
                 'filename)))
            (and name
                 (equal ".docx"
                        (substring name -5 nil))))))
       ("application/x-msword" mm-inline-msword
        (lambda
          (handle)
          (let
              ((name
                (mail-content-type-get
                 (mm-handle-disposition handle)
                 'filename)))
            (and name
                 (equal ".doc"
                        (substring name -4 nil))))))
       ("application/vnd.openxmlformats-officedocument.wordprocessingml.document" mm-inline-docx identity)
       ("application/octet-stream" mm-inline-docx
        (lambda
          (handle)
          (let
              ((name
                (mail-content-type-get
                 (mm-handle-disposition handle)
                 'filename)))
            (and name
                 (equal ".docx"
                        (substring name -5 nil))))))
       ("application/octet-stream" mm-inline-msword
        (lambda
          (handle)
          (let
              ((name
                (mail-content-type-get
                 (mm-handle-disposition handle)
                 'filename)))
            (and name
                 (equal ".doc"
                        (substring name -4 nil))))))
       ("application/msword" mm-inline-msword identity)))

(mapcar (lambda (x) (add-to-list 'mm-inlined-types (car x)))
        my-inline-mime-tests)

(mapcar (lambda (x) (add-to-list 'mm-inline-media-tests x))
        my-inline-mime-tests)

(provide 'init-mail)
