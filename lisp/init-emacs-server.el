(require 'server) ;; has to be a require

;; copied from stsquad (i.e. somewhere on the internet)
(defun my-is-server-running ()
  "Check if an emacs-server process is already running"
  (interactive)
  (let ((socket-path (concat server-socket-dir "/server")))
    (if (functionp 'server-running-p)
        (server-running-p socket-path)
      ; fall back, not as reliable
      (file-exists-p socket-path))))

;; print a message if the server is already started by another process
(defun my-server-status-message ()
  (switch-to-buffer "*scratch*")
  (insert (concat ";; WARNING: emacs-server not started -- already running"))
  (newline-and-indent) (newline-and-indent))

;; start server unless user is root or already running
(if (not (or (string-equal "root" (getenv "USER"))
             (my-is-server-running)))
    (server-start)
  (add-hook 'after-init-hook 'my-server-status-message))

;; C-x k immediately disconnects emacsclient when editing a specified file
(add-hook 'server-switch-hook 
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (local-set-key (kbd "C-x k") 'server-edit)))

(provide 'init-emacs-server)

;; init-emacs-server.el ends here
