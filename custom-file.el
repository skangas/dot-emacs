(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   '("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default))
 '(ecb-options-version "2.32")
 '(fci-rule-color "#383838")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#fabd2f"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#8ec07c"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#555556"))
 '(mairix-file-path "~/Maildir/")
 '(mairix-mail-program 'gnus)
 '(mairix-search-file "mairix_search")
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "ek" :query "tag:ek")
     (:name "desk" :query "tag:desk AND tag:unread")
     (:name "logcheck" :query "tag:logcheck AND not tag:trash AND not tag:deleted")
     (:name "emacs::bugs" :query "tag:emacs::bugs AND tag:unread")
     (:name "emacs" :query "(tag:emacs-devel OR tag:emacs-bugs OR tag:emacs-diffs OR tag:emacs-help) AND tag:unread" :key "e")))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#fb4934")
 '(org-enforce-todo-dependencies t)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m))
 '(package-selected-packages
   '(anki-editor org-babel-eval-in-repl guess-language-mode guess-language ivy browse-kill-ring winum bonjourmadame org-plus-contrib debbugs sublimity org-download org-ac org-bullets org-journal go-mode diff-hl notmuch hydra multiple-cursors iedit rinari smartparens rvm ruby-test-mode robe projectile-rails projectile enh-ruby-mode pymacs macrostep jde auto-package-update auto-complete openwith open-with ido-completing-read+ inf-ruby elisp-slime-nav minitest eruby-mode centered-window xml-rpc haskell-mode google-translate f exec-path-from-shell discover cursor-chg centered-cursor-mode boxquote auto-dim-other-buffers magit dashboard geiser zenburn-theme yaml-mode window-numbering use-package smex paredit nameless elfeed-org auto-compile async))
 '(rustic-ansi-faces
   ["#282828" "#fb4934" "#8ec07c" "#fabd2f" "#268bd2" "#fb2874" "#83a598" "#ebdbb2"])
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'copyright-update nil t)
     (system-time-locale . "sv_SE.UTF-8")
     (truncate-partial-width-windows . t)
     (epa-file-cache-passphrase-for-symmetric-encryption . t)))
 '(send-mail-function 'sendmail-send-it)
 '(smerge-command-prefix "v")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#3a3a3a"))))
 '(fringe ((t (:background "#3F3F3F"))))
 '(org-document-title ((t (:height 1.4 :family "Lucida Grande" :weight bold))))
 '(org-headline-done ((t (:strike-through "#222"))))
 '(org-mode-line-clock ((t (:background "white" :foreground "blue" :box (:line-width -1 :style released-button)))))
 '(variable-pitch ((t (:family "Lucida Grande")))))
