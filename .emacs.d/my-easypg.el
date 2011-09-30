    ;; Disable gpg agent when runing in terminal
    (defadvice epg--start (around advice-epg-disable-agent activate)
      (let ((agent (getenv "GPG_AGENT_INFO")))
        (when (not (display-graphic-p))
          (setenv "GPG_AGENT_INFO" nil))
        ad-do-it
        (when (not (display-graphic-p))
          (setenv "GPG_AGENT_INFO" agent))))
    
(provide 'my-easypg)
