(defadvice call-process (before sk-debug-show-command)
  (if (and program args)
      (message "%s" (concat "call-process: " program " " (sk-join " " args)))))
(ad-update 'sk-debug-show-command)

(defvar sk-debug-call-process-advice-active-p nil
  "Used to track the state of the call-process advice")
(defun sk-debug-call-process-advice-toggle ()
  "Activate advice for `call-process' which prints the command before running it"
  (interactive)
  (cond (sk-debug-call-process-advice-active-p
         (setq sk-debug-call-process-advice-active-p nil)
         (ad-deactivate 'call-process)
         (ad-update 'call-process)
         (message "sk advice deactivated"))
        (t (setq sk-debug-call-process-advice-active-p (ad-activate 'call-process))
           (ad-update 'call-process)
           (message "sk advice activated"))))

(defmacro sk-join (join-char join-list) `(mapconcat 'identity ,join-list ,join-char))

;; THIS SUCKS PRETTY BADLY:

;; (defadvice message (before who-said-that activate)
;;   "Find out who said that thing. and say so."
;;   (let ((trace nil) (n 1) (frame nil))
;;     (while (setq frame (backtrace-frame n))
;;       (setq n     (1+ n) 
;;             trace (cons (cadr frame) trace)) )
;;     (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
;;     (ad-set-args 1 (cons trace (ad-get-args 1)))))

;; ;; To deactivate this, call

;; (ad-disable-advice 'message 'before 'who-said-that)
;; (ad-update 'message)

;; ;; Similarly, to get timestamps:

;; (defadvice message (before when-was-that activate)
;;   "Add timestamps to `message' output."
;;   (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ") 
;;                         (ad-get-arg 0))))

;; ;; And to deactivate this, call 

;; (ad-disable-advice 'message 'before 'when-was-that)
;; (ad-update 'message)

(provide 'sk-macros)

