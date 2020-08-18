(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#11948b")
 '(cua-normal-cursor-color "#596e76")
 '(cua-overwrite-cursor-color "#a67c00")
 '(cua-read-only-cursor-color "#778c00")
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" default))
 '(ecb-options-version "2.32")
 '(fci-rule-color "#405A61")
 '(highlight-changes-colors '("#c42475" "#5e65b6"))
 '(highlight-symbol-colors
   '("#ed7ddb24b29e" "#cd82e29fd17c" "#fc9acadfb443" "#d974d4beddd6" "#df07dfc6b349" "#f76ccd6eaf2a" "#d132db91e15a"))
 '(highlight-symbol-foreground-color "#5d737a")
 '(hl-bg-colors
   '("#d6a549" "#ed6e3e" "#ff6243" "#f46495" "#837bdf" "#6fa5e7" "#66c1b3" "#a8b84b"))
 '(hl-fg-colors
   '("#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9" "#fffce9"))
 '(hl-paren-colors '("#11948b" "#a67c00" "#007ec4" "#5e65b6" "#778c00"))
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(lsp-ui-doc-border "#5d737a")
 '(mairix-file-path "~/Maildir/")
 '(mairix-mail-program 'gnus)
 '(mairix-search-file "mairix_search")
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i" :sort-order oldest-first)
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
 '(objed-cursor-color "#dc322f")
 '(org-enforce-todo-dependencies t)
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-w3m))
 '(package-selected-packages
   '(modus-vivendi-theme modus-operandi-theme ob-async solarized-theme anki-editor org-babel-eval-in-repl guess-language-mode guess-language ivy browse-kill-ring winum bonjourmadame org-plus-contrib debbugs sublimity org-download org-ac org-bullets org-journal go-mode diff-hl notmuch hydra multiple-cursors iedit rinari smartparens rvm ruby-test-mode robe projectile-rails projectile enh-ruby-mode pymacs macrostep jde auto-package-update auto-complete openwith open-with ido-completing-read+ inf-ruby minitest eruby-mode centered-window xml-rpc haskell-mode google-translate f exec-path-from-shell discover cursor-chg centered-cursor-mode boxquote auto-dim-other-buffers magit dashboard geiser zenburn-theme yaml-mode window-numbering use-package smex paredit nameless elfeed-org auto-compile async))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#f4eedb")
 '(pos-tip-foreground-color "#5d737a")
 '(rustic-ansi-faces
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'copyright-update nil t)
     (system-time-locale . "sv_SE.UTF-8")
     (truncate-partial-width-windows . t)
     (epa-file-cache-passphrase-for-symmetric-encryption . t)))
 '(savehist-mode t)
 '(send-mail-function 'sendmail-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#778c00" "#f4eedb" 0.2))
 '(smerge-command-prefix "v")
 '(term-default-bg-color "#fffce9")
 '(term-default-fg-color "#596e76")
 '(weechat-color-list
   '(unspecified "#fffce9" "#f4eedb" "#990001" "#cc1f24" "#4f6600" "#778c00" "#785700" "#a67c00" "#005797" "#007ec4" "#93004d" "#c42475" "#006d68" "#11948b" "#596e76" "#88999b")))

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
