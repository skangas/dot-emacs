(try-require 'w3m-load)

(after 'w3m
  (setq w3m-home-page "http://www.duckduckgo.com/")
  (setq w3m-key-binding 'info)
  (setq w3m-fill-column 80)
  ;; enable cookies
  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies t)
  (setq w3m-profile-directory "~/.w3m")

  (defun skangas-w3m-bindings ()
    (define-key w3m-mode-map [?f] 'w3m-isearch-links)
    ;; (define-key w3m-mode-map "B" 'w3m-view-previous-page)
    ;; (define-key w3m-mode-map "o" 'w3m-history)
    ;; (define-key w3m-mode-map "O" 'w3m-db-history)
    (define-key w3m-mode-map [up] 'previous-line)
    (define-key w3m-mode-map [down] 'next-line)
    (define-key w3m-mode-map [left] 'left-char)
    (define-key w3m-mode-map [right] 'right-char)
    (define-key w3m-mode-map "h" nil)
    (define-key w3m-mode-map "j" nil)
    (define-key w3m-mode-map "k" nil)
    (define-key w3m-mode-map "l" nil)
    (define-key w3m-mode-map "W" nil))
  (add-hook 'w3m-mode-hook 'skangas-w3m-bindings)

  ;; NOT FOUND - STILL SUPPORTED OR WHAT?
  ;; (when (require 'w3m-search)
  ;;   (add-to-list 'w3m-search-engine-alist '("DuckDuckGo" "https://duckduckgo.com/lite/?q=%s&kp=1"))
  ;;   (setq w3m-search-default-engine "DuckDuckGo"))

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

  ;; isearch only for links -- prefix arg to search all text
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
        (point)))))

(provide 'my-w3m)

;; my-w3m.el ends here
