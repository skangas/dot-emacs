;;; init-mail.el                                                 -*- lexical-binding: t; -*-

;; Setup:
;; sudo apt-get install hashcash

(progn
  (setq message-user-fqdn "stefankangas.se") ; used to generate Message-ID
  (setq user-mail-address "stefankangas@gmail.com")
  (setq notmuch-identities '("stefankangas@gmail.com"))
  ;; (setq message-fill-column 72)

  ;; (pcase system-type
  ;;   ('gnu/linux
  ;;    (setq message-send-mail-function 'message-send-mail-with-sendmail)
  ;;    (setq sendmail-program "/usr/bin/msmtp"))
  ;;   ('darwin
  ;;    (load-file "~/org/misc/.osx-sendmail.el")))

  (setq display-time-mail-directory "~/.mail/account.gmail/mail/new")

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program (expand-file-name "~/bin/gmi"))
  (setq message-sendmail-extra-arguments '("send" "--quiet" "-C" "~/.mail/account.gmail"))
  ;; (setq message-sendmail-extra-arguments '("queue" "--quiet" "-C" "~/.mail/account.gmail"))
  (setq notmuch-fcc-dirs nil)
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
                                       "emacs-pretest-bug@gnu.org")))

  ;; allows use of `gnus-dired-attach' (C-c RET C-a)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; Maybe use hashcash.
  (when (executable-find "hashcash")
    (add-hook 'message-send-hook #'mail-add-payment)))

;; check for subject

(defun my/check-for-subject ()
  "Prevent user from sending email without a subject."
  (unless (or (message-field-value "Subject")
              (y-or-n-p "Send email with NO subject line?"))
    (message-goto-subject)
    (user-error "Not sending mail without a subject line")))
(add-hook 'message-send-hook #'my/check-for-subject)

(defun my/check-email ()
  (when (not (string-match (rx "<stefankangas@gmail.com>" eol)
                           (message-field-value "From")))
    (message-goto-from)
    (message-beginning-of-line)
    (kill-line)
    (insert "Stefan Kangas <stefankangas@gmail.com>")
    (user-error "Using wrong email address, press `C-c C-c' again to send")))
(add-hook 'message-send-hook #'my/check-email)

(require 'notmuch)
(with-eval-after-load 'notmuch
  (setq notmuch-show-logo nil)
  (setq notmuch-show-all-tags-list t)
  (setq notmuch-search-oldest-first nil)

  ;; Mark as read.
  (defun sk/notmuch-search-mark-read ()
    "toggle unread tag for message in notmuch search mode."
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread")))
    (notmuch-search-next-thread))

  (define-key notmuch-search-mode-map "v" 'sk/notmuch-search-mark-read)

  ;; Delete message.
  (defun sk/notmuch-show-delete-message ()
    "toggle deleted tag for message in notmuch show mode."
    (interactive)
    (if (member "trash" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-trash"))
      (notmuch-show-tag (list "+trash")))
    (unless (notmuch-show-next-open-message)
      (notmuch-show-next-thread t)))

  (defun sk/notmuch-search-delete-message ()
    "toggle deleted tag for message in notmuch search mode."
    (interactive)
    (if (member "trash" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-trash"))
      (notmuch-search-tag (list "+trash")))
    (notmuch-search-next-thread))

  (setopt notmuch-archive-tags '("-inbox" "-emacs-todo"))

  (define-key notmuch-search-mode-map "h" 'notmuch-search-archive-thread)
  (define-key notmuch-show-mode-map "h" 'notmuch-show-archive-message-then-next-or-next-thread)

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
  "pandoc -r docx -w markdown %s"
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

(defun sk/approve-mailman-post ()
  "Approve a mailman post.  Run in notmuch-mail-show buffer."
  (interactive)
  (cond ((eq major-mode 'notmuch-show-mode)
         (notmuch-show-reply))
        ((eq major-mode 'notmuch-message-mode))
        (t (user-error "sk/approve-mailman-mode: Incorrect major mode")))

  (let ((subject (progn
                   (re-search-forward "> Subject: \\(confirm .*\\)")
                   (match-string 1))))
    (message-goto-subject)
    (message-beginning-of-line)
    (kill-line)
    (insert subject))

  (let ((to (progn
              (re-search-forward "> From: \\(.+-request@marxist.se\\)")
              (match-string 1))))
    (message-goto-to)
    (message-beginning-of-line)
    (kill-line)
    (insert to))

  ;; Approved:
  (newline)
  ;; FIMXE: Fixa för olika e-postlistor...
  (insert "Approved: " sk/mailman-approve-password) ; secret

  ;; Erase body
  (message-goto-body)
  (delete-region (point) (point-max)))

(provide 'init-mail)
