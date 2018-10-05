(require 'rcirc)
(require 'rcirc-late-fix)
(require 'rcirc-notify)

(load-file (expand-file-name "~/.emacs-secrets.el"))

(defface rcirc-nick-in-message `((t (:foreground "red")))
  "My nick when mentioned by others.")
(defface rcirc-my-nick `((t (:foreground ,zenburn-red+1)))
  "My own nick for rcirc.")
(defface rcirc-track-nick '((t (:inherit rcirc-my-nick)))
  "The face used indicate activity directed at you.")
(defface rcirc-track-keyword '((t (:inherit bold)))
  "The face used indicate activity directed at you.")

(setq rcirc-default-nick "skangas"
      rcirc-default-user-name "skangas"
      rcirc-default-full-name "Stefan Kangas"
      rcirc-time-format "%Y-%m-%d %H:%M "
      rcirc-server-alist nil)

(defun irc-start nil
  "Connect to all my networks."
  (interactive)

  ;; rcirc-notify ignore some stuff
  (dolist (name '("lenin" "root" "freenode.proxy" "efnet.proxy" "telecomix.proxy" "perl.proxy"))
    (let ((cur-time (float-time (current-time))))
      (push (cons name cur-time) my-rcirc-notify-nick-alist)))

  ;; bitlbee
  (let (connected)
    (dolist (p (rcirc-process-list))
      (when (string= "bitlbee" (process-name p))
        (setq connected p)))
    (when (not connected)
      (rcirc-connect "localhost" 6667 "skangas" "skangas"
                     "Stefan Kangas" nil nil '(:name "bitlbee"))))

  ;; irc
  (dolist (srv '((6668 . "freenode")
                 (6669 . "efnet")
                 (6670 . "perl")
                 (6671 . "telecomix")))
    (let ((port (car srv))
          (name (cdr srv)))
      (let (connected)
        (dolist (p (rcirc-process-list))
          (when (string= name (process-name p))
            (setq connected p)))
        (when (not connected)
          (rcirc-connect "localhost" port "skangas" "skangas"
                         "Stefan Kangas" nil my-rcirc-server-password
                         `(:name ,name)))))))

;; enable notifications
(rcirc-track-minor-mode 1)

;; disable keepalive pings
(setq rcirc-keepalive-timer t)

;; (eval-after-load "rcirc"
;;   '(defun rcirc-generate-new-buffer-name (process target)
;;      "Return a buffer name based on PROCESS and TARGET.
;; This is used for the initial name given to IRC buffers."
;;      (let ((pname (process-name process)))
;;        (let ((nets '(("localhost<1>" . "freenode")
;;                      ("localhost<2>" . "efnet")
;;                      ("localhost<3>" . "perl")
;;                      ("localhost<4>" . "telecomix"))))
;;          (dolist (net nets)
;;            (when (string-match (car net) pname)
;;              (setq pname (replace-match (cdr net) t t pname)))))
;;        (substring-no-properties
;;         (if target
;;             (concat target "@" pname)
;;           (concat "*" pname "*"))))))

;; (defun my-rcirc-rename-buffer-fun ()
;;   (let ((nets '(("localhost<1>" . "freenode")
;;                 ("localhost<2>" . "efnet"))))
;;     (dolist (net nets)
;;       (let ((old (car net))
;;             (new (cdr net)))
;;         (when (string-match (car net) (buffer-name))
;;           (rename-buffer (replace-match (cdr net) t t (buffer-name))))))))

(defvar my-rcirc-interesting-channels '("#fripost")
  "Channels which should be watched for activity")

(defun my-rcirc-customizations ()
  (when (or (string-match "^*.+*$" (buffer-name))
            (string-match "^&bitlbee@bitlbee$" (buffer-name))
            (and (string-match "^#.+@" (buffer-name))
                 (not (member rcirc-target my-rcirc-interesting-channels))))
    (setq rcirc-ignore-buffer-activity-flag t))
  (flyspell-mode 1))
(add-hook 'rcirc-mode-hook 'my-rcirc-customizations)

;; Omit some notices
;; (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))

;; Logging
(setq rcirc-log-directory "~/.emacs.d/rcirc-logs/"
      rcirc-log-flag t)

;; Turn on logging everything to a special buffer, for debugging.
;; (setq rcirc-debug-flag t)

;; Don't print /away messages.
;; This does not require rcirc to be loaded already,
;; since rcirc doesn't define a 301 handler (yet).
;; (defun rcirc-handler-301 (process cmd sender args)
;;   "/away message handler.")

(set-face-foreground 'rcirc-my-nick "red" nil)

;; Default to utf8, if that fails, use some heuristic to try to decide fallback
(setq rcirc-decode-coding-system 'undecided)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invisible (/mode +i)
(defadvice my-invisible-preference (after rcirc-connect activate)
  "When connecting to a server, set the user mode to +i (invisible)."
  (let ((process ad-return-value)
        (nick (or nick rcirc-default-nick)))
    (rcirc-send-string process (concat "MODE " nick " +i"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamically set fill-column at redisplay time (rcircAutoFillColumn)
(defvar dim:dynamic-fill-column-margin 3
  "Safety margin used to calculate fill-column depending on window-width")

;; dynamically set fill-column at redisplay time
(defun dim:dynamic-fill-column-window (window &optional margin)
  "Dynamically get window's width and adjust fill-column accordingly"
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'rcirc-mode)
      (setq fill-column
            (- (window-width window)
               (or margin dim:dynamic-fill-column-margin))))))

(defun dim:dynamic-fill-column (frame)
  "Dynamically tune fill-column for a frame's windows at redisplay time"
  (walk-windows 'dim:dynamic-fill-column-window 'no-minibuf frame))

(eval-after-load 'rcirc
  '(add-to-list 'window-size-change-functions 'dim:dynamic-fill-column))


;;; Auto-away

(require 'rcirc-auto-away)
(setq rcirc-auto-away-server-regexps '("&bitlbee"))
(setq rcirc-auto-away-after 1800)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-reconnect !!! by Grue @ EmacsWiki
;;; http://www.emacswiki.org/emacs/rcircReconnect

(defun-rcirc-command reconnect (arg)
  "Reconnect the server process."
  (interactive "i")
  (if (buffer-live-p rcirc-server-buffer)
      (with-current-buffer rcirc-server-buffer
	(let ((reconnect-buffer (current-buffer))
	      (server (or rcirc-server rcirc-default-server))
	      (port (if (boundp 'rcirc-port) rcirc-port rcirc-default-port))
	      (nick (or rcirc-nick rcirc-default-nick))
	      channels)
	  (dolist (buf (buffer-list))
	    (with-current-buffer buf
	      (when (equal reconnect-buffer rcirc-server-buffer)
		(remove-hook 'change-major-mode-hook
			     'rcirc-change-major-mode-hook)
                (let ((server-plist (cdr (assoc-string server rcirc-server-alist)))) 
                  (when server-plist 
                    (setq channels (plist-get server-plist :channels))))
                )))
	  (if process (delete-process process))
	  (rcirc-connect server port nick
			 nil
			 nil
			 channels)))))

;;; Attempt reconnection at increasing intervals when a connection is
;;; lost.

(defvar rcirc-reconnect-attempts 0)

;;;###autoload
(define-minor-mode rcirc-reconnect-mode
  nil
  nil
  " Auto-Reconnect"
  nil
  (if rcirc-reconnect-mode
      (progn
	(make-local-variable 'rcirc-reconnect-attempts)
	(add-hook 'rcirc-sentinel-hooks
		  'rcirc-reconnect-schedule nil t))
    (remove-hook 'rcirc-sentinel-hooks
		 'rcirc-reconnect-schedule t)))

(defun rcirc-reconnect-schedule (process &optional sentinel seconds)
  (condition-case err
      (when (and (eq 'closed (process-status process))
		 (buffer-live-p (process-buffer process)))
	(with-rcirc-process-buffer process
	  (unless seconds
	    (setq seconds (exp (1+ rcirc-reconnect-attempts))))
	  (rcirc-print
	   process "my-rcirc.el" "ERROR" rcirc-target
	   (format "scheduling reconnection attempt in %s second(s)." seconds) t)
	  (run-with-timer
	   seconds
	   nil
	   'rcirc-reconnect-perform-reconnect
	   process)))
    (error
     (rcirc-print process "RCIRC" "ERROR" nil
		  (format "%S" err) t))))

(defun rcirc-reconnect-perform-reconnect (process)
  (when (and (eq 'closed (process-status process))
	     (buffer-live-p (process-buffer process))
	     )
    (with-rcirc-process-buffer process
      (when rcirc-reconnect-mode
	(if (get-buffer-process (process-buffer process))
	    ;; user reconnected manually
	    (setq rcirc-reconnect-attempts 0)
	  (let ((msg (format "attempting reconnect to %s..."
			     (process-name process)
			     )))
	    (rcirc-print process "my-rcirc.el" "ERROR" rcirc-target
			 msg t))
	  ;; remove the prompt from buffers
	  (condition-case err
	      (progn
		(save-window-excursion
		  (save-excursion
		    (rcirc-cmd-reconnect nil)))
		(setq rcirc-reconnect-attempts 0))
	    ((quit error)
	     (incf rcirc-reconnect-attempts)
	     (rcirc-print process "my-rcirc.el" "ERROR" rcirc-target
			  (format "reconnection attempt failed: %s" err)  t)
	     (rcirc-reconnect-schedule process))))))))

;; (add-hook 'rcirc-mode-hook
;;           (lambda ()
;;             (if (string-match (regexp-opt '("irc.freenode.net"
;;                                             "irc.inet.tele.dk"
;;                                             "irc.perl.org"
;;                                             "irc.telecomix.org"
;;                                             "localhost"))
;;                               (buffer-name))
;;                 (rcirc-reconnect-mode 1))))

(provide 'init-rcirc)

;; my-rcirc.el ends here
