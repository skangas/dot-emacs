(when (< emacs-major-version 24)
  (when (require 'color-theme)
    (require 'zenburn)
    (color-theme-initialize)
    (color-theme-zenburn)
    ;; bold doesn't work with terminus 12px
    ;; (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
    (set-face-background 'show-paren-match-face "#000")
    (set-face-foreground 'show-paren-match-face "#fff")))

(when (>= emacs-major-version 24)
  (require 'zenburn-theme))

;; Higlight current line ;; DOES NOT WORK PROPERLY
;; (global-hl-line-mode 1)
;; (set-face-background 'hl-line "#393939")
;; (set-face-underline-p 'hl-line nil)

(provide 'my-color-theme)

;; my-color-theme.el ends here
