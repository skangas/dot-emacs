(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   (quote
    ("84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default)))
 '(ecb-options-version "2.32")
 '(mairix-file-path "~/Maildir/")
 '(mairix-mail-program (quote gnus))
 '(mairix-search-file "mairix_search")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "ek" :query "tag:ek")
     (:name "desk" :query "tag:desk AND tag:unread")
     (:name "logcheck" :query "tag:logcheck AND not tag:trash AND not tag:deleted")
     (:name "emacs::bugs" :query "tag:emacs::bugs AND tag:unread"))))
 '(org-enforce-todo-dependencies t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m)))
 '(package-selected-packages
   (quote
    (anki-editor org-babel-eval-in-repl guess-language-mode guess-language ivy browse-kill-ring winum bonjourmadame org-plus-contrib debbugs sublimity org-download org-ac org-bullets org-journal go-mode diff-hl notmuch hydra multiple-cursors iedit rinari smartparens rvm ruby-test-mode robe projectile-rails projectile enh-ruby-mode pymacs macrostep jde auto-package-update auto-complete openwith open-with ido-completing-read+ inf-ruby elisp-slime-nav minitest eruby-mode centered-window xml-rpc haskell-mode google-translate f exec-path-from-shell discover cursor-chg centered-cursor-mode boxquote auto-dim-other-buffers magit dashboard geiser zenburn-theme yaml-mode window-numbering use-package smex paredit nameless elfeed-org auto-compile async)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote before-save-hook)
           (function copyright-update)
           nil t)
     (system-time-locale . "sv_SE.UTF-8")
     (truncate-partial-width-windows . t)
     (epa-file-cache-passphrase-for-symmetric-encryption . t))))
 '(send-mail-function (quote sendmail-send-it)))

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
