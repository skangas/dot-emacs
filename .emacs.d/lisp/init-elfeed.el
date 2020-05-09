;;; init-elfeed.el

(defvar sk/elfeed-filtered-links)
(setq sk/elfeed-filtered-links
      '(("^https://www.theguardian.com/\\(football\\|sport\\|lifeandstyle\\)/" -1000)
        ("^https://www.svt.se/nyheter/uutiset/" -1000)
        ("^https://www.svt.se/nyheter/lokalt/" -1000)
        ("^https://www.svt.se/nyheter/lokalt/\\(vast\\|norrbotten\\)" 1000)
        ("^https://www.bbc.co.uk/sport/" -1000)
        ("^https://www.bbc.com/sport" -1000)))

(defvar sk/elfeed-filtered-words-1000)
(setq sk/elfeed-filtered-words-1000
      (regexp-opt
       '("\\<VM\\>"
         "\\<EM\\>"
         "\\<SM\\>"
         "\\<OS\\>"
         "[Aa]llsvenskan"
         "[ESV]M-guld\\(et\\)?"
         "[ESV]M-brons\\(et\\)?"
         "[ESV]M-silver\\(et\\)?"
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
         "\\(Premier\\|Champions\\) League"
         ;; diverse
         "Eurovision"
         "Nyheter från dagen:"
         "Horoskop – "
         )))

(defvar skangas--elfeed-seen-entry-title
  (make-hash-table :test 'equal))

(defun skangas-elfeed-skip-duplicate-entry (entry)
    "Skip entries with same title as one we seen before to remove duplicates."
    (let ((title (elfeed-entry-title entry)))
      (if (gethash title skangas--elfeed-seen-entry-title)
          (progn (elfeed-untag-1 entry 'unread)
                 (message "Skipping duplicate entry: %s" title))
        (puthash title t skangas--elfeed-seen-entry-title))))

;; (defun skangas-score-elfeed-entry (entry)
;;     (let ((title (elfeed-entry-title entry))
;;           (link (elfeed-entry-link entry))
;;           (categories (elfeed-meta entry :categories))
;;           (content (elfeed-deref (elfeed-entry-content entry)))
;;           (score 0))

;;       (cl-loop for (pattern n) in (list (list sk/elfeed-filtered-words-1000 -1000))
;;                if (string-match pattern title)
;;                do (incf score n)
;;                if (string-match pattern content)
;;                do (incf score n))
;;       ;; LINK
;;       (cl-loop for (pattern n) in sk/elfeed-filtered-links
;;                if (string-match pattern link)
;;                do (incf score n))

;;       ;; Ban categories
;;       (if (memq "Sport" categories) (incf score -1000))

;;       ;; Show result of scoring
;;       (when (not (= score 0))
;;         (message "elfeed scoring: %s - %s (%s)" title score categories))

;;       ;; Store score for later
;;       (setf (elfeed-meta entry :my/score) score)

;;       (cond
;;        ((<= score -1000)
;;         (elfeed-untag-1 entry 'unread))
;;        ((= score 1)
;;         (elfeed-tag entry 'relevant))
;;        ((> score 1)
;;         (elfeed-tag entry 'important)))
;;       entry))

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

(use-package elfeed
  :commands elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
              ("h" . elfeed-search-untag-all-unread) ; more ergonomic keybinding
              ("j" . sk/elfeed-jump/body)
              ("n" . next-line)
              ("p" . 'previous-line)
              ("w" . 'sk/elfeed-search-copy-link)
              ("B" . 'sk/elfeed-search-browse-in-eww))
  :bind (:map elfeed-show-mode-map
              ("B" . 'sk/elfeed-show-browse-in-eww)
              ("w" . 'sk/elfeed-show-copy-link))
  :config
  nil
  (add-hook 'elfeed-new-entry-hook 'skangas-elfeed-skip-duplicate-entry)
  (add-hook 'elfeed-show-mode 'visual-line-mode)
  ;; (add-hook 'elfeed-new-entry-hook 'skangas-score-elfeed-entry)
  )

(use-package elfeed-org
  :ensure t
  :pin "melpa"
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/org/misc/elfeed.org")))

(provide 'init-elfeed)
;; init-elfeed.el ends here
