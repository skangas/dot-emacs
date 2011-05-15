;; Unique buffer names
(require 'uniquify) ;; has to be a require
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; winner-mode
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode

(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
(winner-mode t) ;; turn on the global minor mode

;; BufferSelection
(global-set-key (kbd "C-x C-b") 'bs-show)

;; ISwitchBuffers
;; (iswitchb-mode t)
;; (defadvice iswitchb-kill-buffer (after rescan-after-kill activate)
;;   "*Regenerate the list of matching buffer names after a kill.
;;     Necessary if using `uniquify' with `uniquify-after-kill-buffer-p'
;;     set to non-nil."
;;   (setq iswitchb-buflist iswitchb-matches)
;;   (iswitchb-rescan))
;; (defun iswitchb-rescan ()
;;   "*Regenerate the list of matching buffer names."
;;   (interactive)
;;   (iswitchb-make-buflist iswitchb-default)
;;   (setq iswitchb-rescan t))

; Use C-s and C-r instead
;; (defun iswitchb-local-keys ()
;;   (mapc (lambda (K) 
;; 	  (let* ((key (car K)) (fun (cdr K)))
;; 	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;; 	'(("<right>" . iswitchb-next-match)
;; 	  ("<left>"  . iswitchb-prev-match)
;; 	  ("<up>"    . ignore             )
;; 	  ("<down>"  . ignore             ))))

;; (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;; (require 'ibuffer)
;; (setq ibuffer-saved-filter-groups
;;   '(("default"      
;;             ("Org"
;;              (mode . org-mode))  
;;             ("Mail"
;;               (or
;;                (mode . message-mode)
;;                (mode . mail-mode)
;;                (mode . gnus-group-mode)
;;                (mode . gnus-summary-mode)
;;                (mode . gnus-article-mode)
;;                ))
;;             ("JFS Accounting"
;;               (filename . "src/jfsaccounting/"))
;;             ("Programming"
;;              (or
;;               (mode . c-mode)
;;               (mode . perl-mode)
;;               (mode . python-mode)
;;               (mode . java-mode)
;;               (mode . emacs-lisp-mode)
;;               ))
;;             ("IRC"
;;              (mode . rcirc-mode)))))

;; (add-hook 'ibuffer-mode-hook
;;   (lambda ()
;;     (ibuffer-switch-to-saved-filter-groups "default")))

;; (setq ibuffer-show-empty-filter-groups nil)

(provide 'my-buffers)

;; my-buffers.el ends here
