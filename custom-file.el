(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#689d6a")
 '(cua-normal-cursor-color "#7c6f64")
 '(cua-overwrite-cursor-color "#b57614")
 '(cua-read-only-cursor-color "#98971a")
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default))
 '(default-input-method "spanish-prefix")
 '(ecb-options-version "2.32")
 '(fci-rule-color "#525252")
 '(highlight-changes-colors '("#d3869b" "#8f3f71"))
 '(highlight-symbol-colors
   '("#ed94d1d39b5c" "#d6a5dca3af86" "#eb90bc25933d" "#e1bec426b1e4" "#e40dda889de7" "#ef28c40e9555" "#c667cd42b3b9"))
 '(highlight-symbol-foreground-color "#665c54")
 '(hl-bg-colors
   '("#e29a3f" "#df6835" "#cf5130" "#f598a7" "#c2608f" "#5b919b" "#82cc73" "#c6c148"))
 '(hl-fg-colors
   '("#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7" "#fbf1c7"))
 '(hl-paren-colors '("#689d6a" "#b57614" "#076678" "#8f3f71" "#98971a"))
 '(jdee-db-active-breakpoint-face-colors (cons "#000000" "#80A0C2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#000000" "#A2BF8A"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#000000" "#3f3f3f"))
 '(lsp-ui-doc-border "#665c54")
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
     (:name "emacs" :query "((tag:emacs-devel OR tag:emacs-bugs OR tag:emacs-diffs OR tag:emacs-help) AND (tag:unread OR tag:emacs-todo)" :key "e")))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#C16069")
 '(org-enforce-todo-dependencies t)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m))
 '(package-selected-packages
   '(auctex modus-vivendi-theme modus-operandi-theme ob-async solarized-theme anki-editor org-babel-eval-in-repl guess-language-mode guess-language ivy browse-kill-ring winum bonjourmadame org-plus-contrib debbugs sublimity org-download org-ac org-bullets org-journal go-mode diff-hl notmuch hydra multiple-cursors iedit rinari smartparens rvm ruby-test-mode robe projectile-rails projectile enh-ruby-mode pymacs macrostep jde auto-package-update auto-complete openwith open-with ido-completing-read+ inf-ruby minitest eruby-mode centered-window xml-rpc haskell-mode google-translate f exec-path-from-shell discover cursor-chg centered-cursor-mode boxquote auto-dim-other-buffers magit dashboard geiser zenburn-theme yaml-mode window-numbering use-package smex paredit nameless elfeed-org auto-compile async))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#ebdbb2")
 '(pos-tip-foreground-color "#665c54")
 '(rustic-ansi-faces
   ["#323334" "#C16069" "#A2BF8A" "#ECCC87" "#80A0C2" "#B58DAE" "#86C0D1" "#eceff4"])
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'copyright-update nil t)
     (system-time-locale . "sv_SE.UTF-8")
     (truncate-partial-width-windows . t)
     (epa-file-cache-passphrase-for-symmetric-encryption . t)))
 '(savehist-mode t)
 '(send-mail-function 'sendmail-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#ebdbb2" 0.2))
 '(smerge-command-prefix "v")
 '(term-default-bg-color "#fbf1c7")
 '(term-default-fg-color "#7c6f64")
 '(weechat-color-list
   '(unspecified "#fbf1c7" "#ebdbb2" "#750000" "#9d0006" "#747400" "#98971a" "#8a5100" "#b57614" "#004858" "#076678" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#7c6f64" "#3c3836")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#fbf1c7"))))
 '(org-document-title ((t (:height 1.4 :family "Lucida Grande" :weight bold))))
 '(org-headline-done ((t (:strike-through "#222"))))
 '(org-mode-line-clock ((t (:background "white" :foreground "blue" :box (:line-width -1 :style released-button)))))
 '(variable-pitch ((t (:family "Lucida Grande")))))
