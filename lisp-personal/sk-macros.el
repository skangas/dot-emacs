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

(provide 'sk-macros)

