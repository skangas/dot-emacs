(if (= emacs-major-version 23)
    (progn
      (require 'w3m-load))
      (require 'w3m))

;(setq w3m-profile-directory "~/.w3m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose browser
(setq browse-url-generic-program "/usr/bin/conkeror")
(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (w3m-browse-url url)))
;;(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-browser-function 'choose-browser)

(eval-after-load "w3m"
  '(progn
     (setq w3m-key-binding 'info)
     (setq w3m-fill-column 80) ;; fill columns

     ;; key bindings
     (add-hook 'w3m-mode-hook
               '(lambda ()
                  (define-key w3m-mode-map [?f] 'w3m-isearch-links)
                  (define-key w3m-mode-map "B" 'w3m-view-previous-page)
                  (define-key w3m-mode-map "o" 'w3m-history)
                  (define-key w3m-mode-map "O" 'w3m-db-history)
                  (define-key w3m-mode-map "h" nil)
                  (define-key w3m-mode-map "j" nil)
                  (define-key w3m-mode-map "k" nil)
                  (define-key w3m-mode-map "l" nil)
                  (define-key w3m-mode-map "W" nil)))

     ;; don't enable cookies
     (setq w3m-use-cookies nil)
     (setq w3m-cookie-accept-bad-cookies nil)

     ;; descriptive buffer names
     (add-hook 'w3m-display-hook
               (lambda (url)
                 (rename-buffer
                  (format "*w3m: %s*" (or w3m-current-title
                                          w3m-current-url)) t)))

     ;; remove trailing whitespace
     (add-hook 'w3m-display-hook
               (lambda (url)
                 (let ((buffer-read-only nil))
                   (delete-trailing-whitespace))))

     ;; bookmark to delicious
     ;; (defun ted-delicious-url ()
     ;;   "Bookmark this page with del.icio.us."
     ;;   (interactive)
     ;;   (w3m-goto-url
     ;;    (concat "http://del.icio.us/skangas?"
     ;;            "url="    (w3m-url-encode-string w3m-current-url)
     ;;            "&title=" (w3m-url-encode-string w3m-current-title))))

     ;; (eval-after-load "w3m"
     ;;   '(define-key w3m-info-like-map "a" 'ted-delicious-url))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; isearch only for links -- NECESSARY ADDITION
     (defvar w3m-isearch-links-do-wrap nil
       "Used internally for fast search wrapping.")
     (defun w3m-isearch-links (&optional regexp)
       (interactive "P")
       (let ((isearch-wrap-function
              #'(lambda ()
                  (setq w3m-isearch-links-do-wrap nil)
                  (if isearch-forward
                      (goto-char (window-start))
                    (goto-char (window-end)))))
             (isearch-search-fun-function
              #'(lambda () 'w3m-isearch-links-search-fun))
             post-command-hook               ;inhibit link echoing
             do-follow-link
             (isearch-mode-end-hook
              (list  #'(lambda nil
                         (when (and (not isearch-mode-end-hook-quit)
                                    (w3m-anchor))
                           (setq do-follow-link t))))))
         (setq w3m-isearch-links-do-wrap t)
         (isearch-mode t
                       regexp
                       ;; fast wrap
                       #'(lambda nil
                           (if isearch-success
                               (setq w3m-isearch-links-do-wrap t)
                             (when w3m-isearch-links-do-wrap
                               (setq w3m-isearch-links-do-wrap nil)
                               (setq isearch-forward
                                     (not isearch-forward))
                               (isearch-repeat isearch-forward))))
                       t)
         (when do-follow-link
           (w3m-view-this-url))))

     (defun w3m-isearch-links-search-fun (string &optional bound no-error)
       (let* (isearch-search-fun-function
              (search-fun  (isearch-search-fun))
              error
              (bound  (if isearch-forward
                          (max (or bound 0)
                               (window-end))
                        (min (or bound (window-start))
                             (window-start)))))
         (condition-case err
             (while (and (apply search-fun (list string bound))
                         (not (w3m-anchor (point)))))
           (error (setq error err)))
         (if error
             (if (not no-error)
                 (signal (car error) (cadr error)))
           (point))))))

(provide 'my-w3m)

;; my-w3m.el ends here
