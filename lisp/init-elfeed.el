;;; init-elfeed.el

(defvar sk/elfeed-filtered-links)
(setq sk/elfeed-filtered-links
      '(
        ("^https://www.bbc.co.uk/sport/" -1000)
        ("^https://www.bbc.com/sport" -1000)
        ("^https://www.svt.se/nyheter/lokalt/" -1000)
        ("^https://www.svt.se/nyheter/lokalt/\\(vast\\|norrbotten\\)" 1000)
        ("^https://www.svt.se/nyheter/uutiset/" -1000)
        ("^https://www.theguardian.com/food/" -1000)
        ("^https://www.theguardian.com/football/" -1000)
        ("^https://www.theguardian.com/lifeandstyle/" -1000)
        ("^https://www.theguardian.com/sport/" -1000)
        ("^https://www.theguardian.com/stage/" -1000)
        ))

(defvar sk/elfeed-filtered-words-1000)
(setq sk/elfeed-filtered-words-1000
      `("VM"
        "EM"
        "SM"
        "OS"
        "[Aa]llsvenskan"
        "[ESV]M-guld\\(et\\)?"
        "[ESV]M-brons\\(et\\)?"
        "[ESV]M-silv\\(er\\|et\\)?"
        "[ESV]M-genrep\\(et\\)?"
        "[ESV]M-kvaltrupp\\(en\\)?"
        "[ESV]M-lag\\(e[tn]\\)?"
        "[ESV]M-kval\\(et\\)?"
        "bortaplan"
        "bortasegern?"
        "damidrott\\(en\\)?"
        "finalserien?"
        "fotboll"
        "fotbollslag\\(et\\)?"
        "fotbollsspelaren?"
        "förbundskapten\\(en\\)?"
        "guldmålvakt\\(en\\)?"
        "handboll"
        "handbollsligan?"
        "handbollsmästaren?"
        "herridrott\\(en\\)?"
        "hemmaplan"
        "hemmapublik\\(en\\)?"
        "\\(is\\)?hockeylag\\(et\\|en\\)?"
        "\\(is\\)?hockeyspelaren?"
        "kvartsfinal\\(en\\)?"
        "lagkapten\\(en\\)?"
        "landslag\\(e[nt]\\)?"
        "löparstjärnan?"
        "matchens enda mål"
        "matchserien?"
        "medaljhopp\\(et\\)"
        "målform"
        "passningspoäng"
        "poängkung\\(en\\)?"
        "poängligan?"
        "proffshockeyn?"
        "semifinal\\(en\\)?"
        "semifinallag\\(e[nt]\\)?"
        "skrällag\\(e[nt]?\\)"
        "slutspel\\(e[nt]?\\)"
        ;; Finska lag
        "HIFK"
        "BK-46"
        "Smålejon\\(en\\)?"
        "Lejon\\(en\\)?"
        "IFK Mariehamn"
        "Sjundeå IF"
        ;; engelska sporttermer
        "Ajax"
        "Boston Bruins"
        "Chicago Blackhawks"
        "Columbus Blue Jackets"
        "Dallas Stars"
        "Diamond League"
        "NHL"
        "New York Giants"
        "Oakland Raiders"
        "Stanley Cup"
        "World Cup"
        "World\\( Snooker\\)? Championship"
        "Messi"
        "Manchester City"
        "\\(Premier\\|Champions\\) League"
        ;; diverse
        "Eurovision"
        "Nyheter från dagen:"
        "Horoskop – "
        ))

(defvar skangas--elfeed-seen-entry-title
  (make-hash-table :test 'equal))

(defvar elfeed-score-log-buffer "*Elfeed-score*")

(defun elfeed-score-log (msg &rest args)
  (with-current-buffer (get-buffer-create elfeed-score-log-buffer)
    (save-excursion
      (goto-char (point-max))
      (insert (apply #'format (concat (format "elfeed-score: %s\n" msg)) args)))))

(defun skangas-elfeed-skip-duplicate-entry (entry)
  "Skip entries with same title as one we seen before to remove duplicates."
  (let ((title (elfeed-entry-title entry)))
    (if (gethash title skangas--elfeed-seen-entry-title)
        (progn (elfeed-untag-1 entry 'unread)
               (message "Skipping duplicate entry: %s" title))
      (puthash title t skangas--elfeed-seen-entry-title))))

(defun skangas-score-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (link (elfeed-entry-link entry))
        (categories (elfeed-meta entry :categories))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))

    (elfeed-score-log "SCORING: %s @ %s" title link)

    (cl-loop for (pattern score) in (list (list sk/elfeed-filtered-words-1000 -1000))
             if (string-match pattern title)
             do (progn
                  (elfeed-score-log "%s %s" score pattern)
                  (incf score score))
             if (string-match pattern content)
             do (progn
                  (elfeed-score-log "%s %s" score pattern)
                  (incf score score)))
    ;; LINK
    (cl-loop for (pattern score) in sk/elfeed-filtered-links
             if (string-match pattern link)
             do (progn
                  (elfeed-score-log "%s %s" score pattern)
                  (incf score score)))

    ;; Ban categories
    (if (memq "Sport" categories) (incf score -1000))

    ;; Show result of scoring
    (when (not (= score 0))
      (message "elfeed-score: %s - %s (%s)" title score categories))

    ;; Store score for later
    (setf (elfeed-meta entry :my/score) score)

    (cond
     ((<= score -1000)
      (elfeed-untag-1 entry 'unread))
     ((= score 1)
      (elfeed-tag entry 'relevant))
     ((> score 1)
      (elfeed-tag entry 'important)))
    entry))

(defun sk/elfeed-search-copy-link (entry)
  (interactive (list (elfeed-search-selected :single)))
  (let ((link (elfeed-entry-link entry)))
    (kill-new link)
    (message link)))

(defun sk/elfeed-search-browse-in-eww (entry)
  (interactive (list (elfeed-search-selected :single)))
  (let ((link (elfeed-entry-link entry)))
    (eww-browse-url link)))

(defun sk/elfeed-show-copy-link ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (kill-new link)
    (message link)))

(defun sk/elfeed-show-browse-in-eww ()
  (interactive)
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (eww-browse-url link)))

(defun sk/elfeed-show-share-link ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((title (elfeed-entry-title elfeed-show-entry))
          (source (elfeed-feed-title (elfeed-entry-feed elfeed-show-entry)))
          (body (buffer-substring (point-min) (point-max)))
          (url (elfeed-entry-link elfeed-show-entry)))
      (message-mail)
      ;; Compose message.
      (insert "foo@example.org")
      (message-goto-subject)
      (insert title " | " source)
      (message-goto-body)
      (insert url "\score\score" body)
      ;; Cleanup.
      (message-goto-body)
      (re-search-forward "^Feed: \\(.*\\)\score")
      (replace-match "")
      (re-search-forward "^Tags: \\(.*\\)\score")
      (replace-match "")
      (re-search-forward "^Link: \\(.*\\)\score")
      (replace-match "")
      ;; Finish.
      (message-goto-body))))

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("h" . elfeed-search-untag-all-unread) ; more ergonomic keybinding
              ;; ("j" . sk/elfeed-jump/body)
              ("n" . next-line)
              ("p" . 'previous-line)
              ("w" . 'sk/elfeed-search-copy-link)
              ("B" . 'sk/elfeed-search-browse-in-eww))
  :bind (:map elfeed-show-mode-map
              ("h" . 'elfeed-show-next)
              ("B" . 'sk/elfeed-show-browse-in-eww)
              ("w" . 'sk/elfeed-show-copy-link)
              ("S" . 'sk/elfeed-show-share-link))
  :config
  (add-hook 'elfeed-new-entry-hook 'skangas-elfeed-skip-duplicate-entry)
  (add-hook 'elfeed-show-mode 'visual-line-mode)
  (add-hook 'elfeed-new-entry-hook 'skangas-score-elfeed-entry)

  (setq elfeed-sort-order 'descending)

  (with-eval-after-load 'hydra
    (defhydra sk/elfeed-jump ()
      "filter"
      ("1" (elfeed-search-set-filter "@6-months-ago +svenska +nyheter +unread") "svenska")
      ("2" (elfeed-search-set-filter "@6-months-ago +imt +unread") "imt")
      ("3" (elfeed-search-set-filter "@6-months-ago +engelska +nyheter +unread") "engelska")
      ("4" (elfeed-search-set-filter "@6-months-ago +bloggar +unread") "bloggar")

      ("7" (elfeed-search-set-filter "@6-months-ago +youtube unread") "youtube")
      ("8" (elfeed-search-set-filter "@6-months-ago +imt +español +unread") "imt+español")
      ("9" (elfeed-search-set-filter "@6-months-ago +español +unread") "español")

      ("score" (elfeed-search-set-filter "@6-months-ago +nyheter +unread") "nyheter")
      ("t" (elfeed-search-set-filter "@6-months-ago +tech +unread") "tech")

      ;; ("M" elfeed-toggle-star "Mark")
      ("A" (elfeed-search-set-filter "@6-months-ago") "All")
      ("T" (elfeed-search-set-filter "@1-day-ago") "Today")
      ("Q" bjm/elfeed-save-db-and-bury "Quit Elfeed" :color blue)
      ("q" nil "quit" :color blue)))

  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  (define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
  (define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command))

;; (setq elfeed-show-entry-switch #'switch-to-buffer)

;; (defun elfeed-display-buffer (buf &optional act)
;;   nil
;;   ;; (pop-to-buffer buf)
;;   ;; (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height))))
;;   )

(provide 'init-elfeed)
