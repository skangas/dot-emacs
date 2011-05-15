;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tramp

(eval-after-load "tramp"
  '(progn
     (setq tramp-default-method "ssh")

     (set-default 'tramp-default-proxies-alist
                  '((".*" "\\`root\\'" "/ssh:%h:")))
     
     ;; don't backup files edited in tramp using sudo or su -- we don't want to
     ;; spread secret root files around.
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
                  (not
                   (let ((method (file-remote-p name 'method)))
                     (when (stringp method)
                       (member method '("su" "sudo"))))))))

     (defun xwl-revert-buffer-with-sudo ()
       "Revert buffer using tramp sudo.
    This will also reserve changes already made by a non-root user."
       (interactive)
       (let ((f (buffer-file-name)))
         (when f
           (let ((content (when (buffer-modified-p)
                            (widen)
                            (buffer-string))))
             (if (file-writable-p f)
                 (revert-buffer)
               (kill-buffer (current-buffer))
               (if (file-remote-p f)
                   (find-file
                    (replace-regexp-in-string "^\\/[^:]+:" "/sudo:" f))
                 (find-file (concat "/sudo::" f)))
               (when content
                 (let ((buffer-read-only nil))
                   (erase-buffer)
                   (insert content))))))))))
  
(provide 'my-tramp)
