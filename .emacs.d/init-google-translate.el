(require 'google-translate)

;; TODO: Add these to the google-translate.el
(autoload google-translate-at-point "google-translate.el" nil t)
(autoload google-translate-query-at-point "google-translate.el" nil t)

(eval-after-load 'google-translate
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "sv"))

(provide 'init-google-translate)
