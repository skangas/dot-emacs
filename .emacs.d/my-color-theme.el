(when (require 'color-theme)
  (require 'zenburn)
  (color-theme-initialize)
  (color-theme-zenburn))

;; Higlight current line
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#393939")
;; (set-face-underline-p 'hl-line nil)

;; Show parenthesis
(show-paren-mode 1)
(set-face-background 'show-paren-match-face "#000")
(set-face-foreground 'show-paren-match-face "#fff")
;; bold doesn't work with terminus 12px
;; (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)

(provide 'my-color-theme)

;; my-color-theme.el ends here
