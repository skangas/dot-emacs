;;; General settings
(require 'ansi-color)


;;;; Various configuration settings
;; FIXME: add visual line mode to all modes where it makes sense

(defmacro run-if-fboundp (arg)
  (if (fboundp (car arg)) arg))
(unless (eq window-system 'ns)
  (run-if-fboundp (menu-bar-mode -1)))     ; No menu
(run-if-fboundp (scroll-bar-mode -1))      ; No scrollbar
(run-if-fboundp (tool-bar-mode -1))        ; No toolbar
(run-if-fboundp (mouse-wheel-mode 1))      ; Enable mousewheel
(run-if-fboundp (column-number-mode 1))    ; Show column number
(run-if-fboundp (line-number-mode 1))      ; Show line number
(run-if-fboundp (auto-image-file-mode 1))  ; View images in emacs
(run-if-fboundp (display-time-mode 1))
(save-place-mode 1)                        ; Use `save-place-mode'
(savehist-mode 1)                          ; Use `savehist-mode'
(global-hl-line-mode 1)                    ; Use hl-line-mode globally
(show-paren-mode 1)

;; * Font Lock mode, Auto Compression mode, and File Name Shadow Mode
;;   are enabled by default.

;; Change all yes or no prompt to y or n prompts
(fset 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 500)
(setq scroll-margin 2)
(setq scroll-step 0)
(setq scroll-preserve-screen-position nil)

(setq user-full-name "Stefan Kangas"
      user-mail-address "stefan@marxist.se"
      require-final-newline t                        ; Make sure text files end in a newline
      Man-width 80                                   ; Limit man to 80 character width
      message-send-mail-partially-limit nil          ; Never split emails

      kill-ring-max (* kill-ring-max 2)
      ;; scroll-conservatively most-positive-fixnum     ; Always scroll one line at a time
      scroll-preserve-screen-position t              ; Affects Page-up Page-down
      mouse-yank-at-point t                          ; Yank at point, even in X
      lazy-highlight-initial-delay 0.15              ; Seconds to wait before isearch highlights
      ffap-machine-p-known 'reject                   ; stop ffap from pinging random hosts
      save-interprogram-paste-before-kill t
      apropos-do-all t

      ;; choose browser
      browse-url-browser-function #'browse-url-generic
      browse-url-generic-program (if (eq system-type 'darwin) "open" "firefox")

      frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name)))

      ;; calendar
      calendar-date-style 'european          ; Use European calendar
      ;; holidays
      calendar-mark-holidays-flag t
      calendar-holidays nil
      holiday-bahai-holidays nil
      holiday-christian-holidays nil
      holiday-dragon-holidays nil
      holiday-general-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-solar-holidays nil)

(setq sk/video-types
      (concat (regexp-opt '(".asf" ".avi" ".f4v"
                            ".flv" ".m4a" ".m4v"
                            ".mkv" ".mov" ".mp4"
                            ".m2ts" ".mpeg" ".mpg"
                            ".ogv" ".wmv" ".webm")) "\\'"))

(setq history-delete-duplicates t)
(setq help-window-select t)
(setq track-eol t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        last-kbd-macro
        kmacro-ring
        shell-command-history
        Info-history-list))

;; TODO: Could/should this be added to Emacs itself?
(when (>= emacs-major-version 27)
  (defun dotfiles--gc-on-last-frame-out-of-focus ()
    "GC if all frames are inactive."
    (if (seq-every-p #'null (mapcar #'frame-focus-state (frame-list)))
        (garbage-collect)))
  (add-function :after after-focus-change-function
                #'dotfiles--gc-on-last-frame-out-of-focus))

;; - spaces are used for delimiters within use-package statements
;; - \n gives correct line count for page
(setq page-delimiter "^ *\C-l\n")

(setq enable-recursive-minibuffers t)

;; This is very slow, due to Terminus font? -- skangas @ 2019-11-05
;; (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437-dos))

(setq-default fill-column 80      ;; note to self: use M-q and C-u 78 C-x f
              indent-tabs-mode nil                   ; Always indent using spaces, never tabs
              indicate-empty-lines t                 ; Show empty lines at end of file
              indicate-buffer-boundaries 'left)      ; Show markers indicating buffer limits

(setq holiday-swedish-holidays
      '((holiday-fixed 1 1 "Nyårsdagen")
        (holiday-fixed 1 6 "Trettondedag jul")
        (holiday-fixed 5 1 "Första maj")
        (holiday-fixed 6 1 "Sveriges nationaldag")
        (holiday-fixed 1 25 "Juldagen")
        (holiday-fixed 1 26 "Annandag jul")
        (holiday-fixed 1 31 "Nyårsafton")))
(setq calendar-holidays holiday-swedish-holidays)

(setq world-clock-list
      '(("America/Los_Angeles" "Seattle")
        ("America/Chicago" "Chicago")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Stockholm" "Stockholm")
        ("Europe/Rome" "Rome")
        ("Asia/Karachi" "Karachi")
        ("Asia/Shanghai" "Shanghai")
        ("Australia/Sydney" "Sydney")))

(setq sv-hide-some-holidays t)
(require 'sv-kalender)

(require 'uniquify) ;; has to be a require
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; (setq use-dialog-box nil) ;; DON'T DO THIS! Will unfortunately sometimes crash emacs

(add-hook 'before-save-hook 'time-stamp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup files
(setq version-control t         ; use versioned backups
      kept-old-versions 255
      kept-new-versions 1024
      delete-old-versions t
      backup-by-copying t       ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.emacs.d/cache/saves")))    ; don't litter

;; Delete old and big backup files that just wastes space
(let ((bak-dir (expand-file-name "~/.emacs.d/cache/saves")))
  (when (and (file-exists-p bak-dir)
             (file-directory-p bak-dir))
    (start-process (concat "delete old backup files in " bak-dir)
                   "*Messages*" "find" bak-dir "-size" "+1M" "-mtime" "+90" "-delete")))

;; Change cursor color depending on context (EmacsNiftyTricks)
(setq my/set-cursor-color-color "")
(setq my/set-cursor-color-buffer "")
(defun my/set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "#8888FF"
           (if overwrite-mode "#000000"
             "#FF0000"))))
    (unless (and
             (string= color my/set-cursor-color-color)
             (string= (buffer-name) my/set-cursor-color-buffer))
      (set-cursor-color (setq my/set-cursor-color-color color))
      (setq my/set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'my/set-cursor-color-according-to-mode)

;; hexcolour
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property (match-beginning 0)
                           (match-end 0)
                           'face (list :background
                                       (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

(add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)
(add-hook 'conf-xdefaults-mode-hook 'hexcolour-add-to-font-lock)

;; If the *scratch* buffer is killed, recreate it automatically
(with-current-buffer (get-buffer-create "*scratch*")
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;;; ediff
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)


;;;; packages

(use-package abbrev
  :config
  (setq save-abbrevs t
        abbrev-file-name "~/org/.abbrev_defs")

  ;; reads the abbreviations file on startup
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  ;; Enable abbrev-mode in text and derived modes
  (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
  ;; (add-hook 'emacs-lisp-mode-hook (lambda () (abbrev-mode 1)))
  ;; (add-hook 'erc-mode-hook (lambda () (abbrev-mode 1)))
  )

(use-package avy
  :ensure t
  :bind (("C-å" . avy-goto-char-timer)
         ("C-Å" . avy-goto-char)
         ("C-x SPC" . avy-mark)
         ("M-g f" . avy-goto-line)))

(use-package ag
  :ensure t)

(use-package async
  :ensure t
  :config
  (dired-async-mode 1))

;; (use-package centered-cursor-mode
;;   :ensure t
;;   :config
;;   ;; center cursor in info-mode
;;   (defun my-info-mode-hook-center-cursor ()
;;     (centered-cursor-mode))
;;   (setq Info-mode-hook 'my-info-mode-hook-center-cursor))

;; (use-package auto-dim-other-buffers
;;   :pin "melpa"
;;   :ensure t
;;   :config
;;   (auto-dim-other-buffers-mode t))

(use-package boxquote
  :ensure t)

(use-package centered-window
  :pin "melpa"
  :ensure t)

(use-package dash
  :ensure t
  :defer t)

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package diminish
  :ensure t
  :config
  (with-eval-after-load 'auto-dim-other-buffers
    (diminish 'auto-dim-other-buffers-mode ""))
  (diminish 'abbrev-mode "Ab")
  (diminish 'eldoc-mode "")
  (with-eval-after-load 'anzu
    (diminish 'anzu-mode ""))
  (with-eval-after-load 'company
    (diminish 'company-mode "comp"))
  (with-eval-after-load 'enh-ruby-mode
    (diminish 'enh-ruby-mode "Ruby"))
  (with-eval-after-load 'gcmh
    (diminish 'gcmh-mode ""))
  (with-eval-after-load 'paredit
    (diminish 'paredit-mode "PEd"))
  (with-eval-after-load 'minitest
    (diminish 'minitest-mode "MT"))
  (with-eval-after-load 'robe
    (diminish 'robe-mode "Ro"))
  (with-eval-after-load 'ruby-test-mode
    (diminish 'ruby-test-mode "RT")))

(use-package dired                      ; (built-in)
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)
              ("," . dired-hide-details-mode)
              ("å" . dired-sk/open-media-dwim)
              ("C-i" . image-dired-here))
  :config
  (if (eq system-type 'darwin)
      (setq dired-listing-switches "-lAFh")
    (setq dired-listing-switches "-lAFh --group-directories-first"))
  (setq dired-dwim-target t)           ; Try to guess a default target directory
  (setq dired-isearch-filenames 'dwim) ; Search filenames only
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  (setq dired-make-directory-clickable t) ; 29.1

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (define-minor-mode dired-hide-dotfiles-mode
    "Toggle showing dot-files."
    :lighter " Hide"
    :init-value nil
    (if (not (eq major-mode 'dired-mode))
        (progn
          (error "Doesn't seem to be a Dired buffer")
          (setq dired-hide-dotfiles-mode nil))
      (if dired-hide-dotfiles-mode
          (setq dired-actual-switches "-lh")
        (setq dired-actual-switches "-lAh"))
      (revert-buffer)))

  (defun dired-sk/open-media-dwim ()
    "Make a preview buffer for all images in current directory and display it."
    (interactive)
    (let* ((file (dired-get-file-for-visit))
           (cmd (if (string-match sk/video-types file)
                    "mpv * &"
                  "feh -F -Z -r -z * &") ))
      (message cmd)
      (dired-do-shell-command cmd nil (list file))))

  (require 'dired-x)
  (push `(,sk/video-types "mpv")
        dired-guess-shell-alist-default)

  (require 'dired-aux)
  (setq dired-create-destination-dirs 'ask))

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))

(use-package embark
  :ensure t
  :pin "gnu"
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package epa-file
  :config
  ;;(setq epa-armor t)
  ;; Disable gpg agent when runing in terminal
  (defadvice epg--start (around advice-epg-disable-agent activate)
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (when (not (display-graphic-p))
        (setenv "GPG_AGENT_INFO" nil))
      ad-do-it
      (when (not (display-graphic-p))
        (setenv "GPG_AGENT_INFO" agent)))))

(use-package erc ; built-in
  :config
  (add-hook 'erc-mode-hook #'abbrev-mode))

(use-package eshell ; built-in
  :config
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(use-package eww                        ; built-in
  :config
  (setq shr-width 80)
  (defun sk/my-eww-mode-hook ()
    ;; Set line-spacing to at least N.
    (when (natnump line-spacing)
      (setq line-spacing (max line-spacing 5))))
  (add-hook 'eww-mode-hook 'sk/my-eww-mode-hook)

  ;; Fix <mark> tags for www.rae.es
  (defun shr-tag-mark (dom)
    (shr-generic dom)
    ;; Hack to work around bug in libxml2 (Bug#48211):
    ;; https://gitlab.gnome.org/GNOME/libxml2/-/issues/247
    (insert " "))

  (defun sk/eww-fix-up-whitespace-for-rae ()
    (save-excursion
      (let ((buffer-read-only nil))
        (goto-char (point-min))
        (if (string= (substring (plist-get eww-data :url) 0 19)
                     "https://dle.rae.es/")
            (while (re-search-forward " \\([.,]\\)" nil t)
              (replace-match "\\1"))))))
  (add-hook 'eww-after-render-hook #'sk/eww-fix-up-whitespace-for-rae)

  (defun sk/eww-move-point-in-place ()
    (when (string-match "www.marxist.com/.*\.htm" (plist-get eww-data :url))
      (search-forward "Details " nil t)
      (recenter 0)))
  (add-hook 'eww-after-render-hook #'sk/eww-move-point-in-place))

(use-package flyspell :ensure nil
  :config
  (setq flyspell-issue-welcome-flag nil)
  ;; Non-nil means that flyspell uses M-TAB to correct word.
  (setq flyspell-use-meta-tab nil)
  ;; If non-nil, add correction to abbreviation table.
  (setq flyspell-abbrev-p t)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

;; (use-package google-translate
;;   :ensure t
;;   :bind (("C-c t" . google-translate-at-point)
;;          ("C-c T" . google-translate-query-translate)))

;; (use-package guess-language-mode
;;   :ensure t
;;   :config
;;   (setq guess-language-languages '(en sv)))

(use-package ibuffer
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Main"
            (or
             (mode . image-mode)
             (mode . dired-mode)
             (filename . "/mnt/usb/seed/")))
           ("Work"
            (or
             (mode . org-agenda-mode)
             (name . "org/")
             (name . "\*mentor\*")))
           ("Text Files"
            (or
             (mode . org-mode)
             (mode . text-mode)
             ))
           ("Email"
            (or
             (mode . notmuch-search-mode)
             (mode . notmuch-show-mode)
             (mode . notmuch-hello-mode)
             (mode . message-mode)
             (mode . mail-mode)
             (mode . gnus-group-mode)
             (mode . gnus-summary-mode)
             (mode . gnus-article-mode)))
           ("Emacs Configuration"
            (or (filename . "\\.emacs\\.d")
                (filename . "\\.emacs")))
           ("Mentor"
            (filename . "wip/mentor"))
           ("Emacs"
            (or
             (filename . "~/wip/emacs")))
           ("Emacs Lisp"
            (mode . emacs-lisp-mode))
           ("Programming"
            (or
             (mode . ag-mode)
             (mode . grep-mode)
             (mode . compilation-mode)
             (mode . c-mode)
             (mode . perl-mode)
             (mode . cperl-mode)
             (mode . python-mode)
             (mode . java-mode)
             (mode . sh-mode)
             (mode . haskell-mode)))
           ("Configuration"
            (or
             (mode . conf-unix-mode)))
           ("IRC"
            (mode . rcirc-mode)))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t))

(use-package iedit
  :ensure t)

(use-package image-dired                ; (built-in)
  :config
  (setq image-dired-dir "~/.emacs.d/cache/image-dired/")
  (setq image-dired-thumb-width  150
        image-dired-thumb-height 150)

  (defun image-dired-here ()
    "Make a preview buffer for all images in current directory and display it."
    (interactive)
    (image-dired default-directory)))

(use-package image-mode                 ; (built-in)
  :no-require t
  :bind (:map image-mode-map
              ("SPC" . #'image-next-file)
              ("V" . #'sk/image-mode-toggle-resized)))

(use-package ioccur
  :pin "gnu"
  :ensure t)

(use-package ispell
  :config
  ;; FIXME: temporary workaround
  ;; (setq ispell-program-name "hunspell")
  ;; (setq ispell-dictionary "en_US,sv_SE,es_ES")
  ;; (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic "en_US,sv_SE,es_ES")

  ;; (setq ispell-extra-args '("--sug-mode=ultra"))
  ;; (setq ispell-dictionary "swedish"
  ;;       ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it

  (setq ispell-silently-savep t)
  (setq flyspell-use-global-abbrev-table-p t)
  )

(use-package marginalia
  :pin "gnu"
  :ensure t
  :init
  (marginalia-mode 1))

(use-package midnight                   ; (built-in)
  :init
  (midnight-mode 1)
  (setq clean-buffer-list-delay-general 7) ; default is 3 days
  (midnight-delay-set 'midnight-delay "06:00")
  (timer-activate midnight-timer))

(use-package markdown-mode
  :ensure t
  :defer 300 ; I rarely use this
  :mode ("\\.md\\'" . gfm-mode))

(use-package mpc                        ; (built-in)
  :config
  (setq mpc-mpd-music-directory "~/music"))

(use-package multiple-cursors
  :ensure t)

(use-package openwith                   ; open files using external helpers
  :ensure t
  :pin "melpa"
  :config
  (openwith-mode t)

  (setq openwith-associations
        `((,sk/video-types "mpv --cache=50000" (file))
          ("\\(?:\\.img\\|\\.iso\\)\\'" "mpv" ("dvd://" "-dvd-device" file))
          ("\\.azw3\\'" "calibre" (file))
          ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
          ;; ("\\.mp3\\'" "mplayer" (file))
          ;; ("\\.pdf\\'" "evince" (file))
          ))

  (when (version< emacs-version "27")
    ;; Do not warn about big files for openwith files
    (defadvice abort-if-file-too-large (around my-do-not-prompt-for-big-media-files
                                               (size op-type filename))
      (if (and openwith-mode
               (equal op-type "open")
               (some (lambda (oa)
                       (save-match-data (string-match (car oa) filename)))
                     openwith-associations))
          (let ((large-file-warning-threshold nil))
            ad-do-it)
        ad-do-it))
    (ad-deactivate 'abort-if-file-too-large)
    (ad-activate 'abort-if-file-too-large)))

(use-package orderless
  :pin "gnu"
  :ensure t
  :custom (completion-styles '(orderless basic)))

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))

(use-package recentf                    ; built-in
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 100)
  (recentf-save-file "~/.emacs.d/cache/recentf")
  (recentf-exclude `(,(rx bos "/" (or "home" "Users") "/skangas/"
                          (or (seq "org/" (* any))
                              (seq ".emacs.bmk" eos))))))

(use-package tramp                      ; built-in
  :config
  ;; don't backup any remote files:
  ;; (info "(tramp) Auto-save File Lock and Backup")
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(use-package undo-tree
  :pin "gnu"
  :ensure t)

(use-package visual-fill-column
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-add-major-mode-key-based-replacements 'enh-ruby-mode
    "C-c r !" "Run rails"
    "C-c r T" "Go to test/toggle"
    "C-c r t" "Go to test/search"))

(use-package winner                     ; (built-in)
  :bind (("<C-s-left>" . winner-undo)
         ("<C-s-right>" . winner-redo))
  :config
  (setq winner-dont-bind-my-keys t)    ; default bindings conflict with org-mode
  (winner-mode 1))

(use-package winum
  ;; Replaces window-numbering.el
  :ensure t
  :pin "melpa"
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9))
  :config
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
  (setq winum-scope 'frame-local)
  (winum-mode))

(provide 'init-general)
