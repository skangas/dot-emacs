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
;; * Font Lock mode, Auto Compression mode, and File Name Shadow Mode
;;   are enabled by default.

;; Change all yes or no prompt to y or n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Use hl-line-mode globally.
(global-hl-line-mode 1)

;; Increase min bits to 2048
;; https://lists.gnu.org/archive/html/emacs-devel/2018-06/msg00718.html
;; (setq gnutls-min-prime-bits (max 2048 gnutls-min-prime-bits))

(setq scroll-conservatively 500)
(setq scroll-step 0)
(setq scroll-preserve-screen-position nil)

(setq user-full-name "Stefan Kangas"
      user-mail-address "stefan@marxist.se"
      inhibit-startup-message t                      ; No startup message
      visible-bell t                                 ; No audible bell
      display-time-24hr-format t                     ; Show 24hr clock when it's shown
      bookmark-save-flag 1                           ; Save bookmarks immediately when added
      require-final-newline t                        ; Make sure text files end in a newline
      Man-width 80                                   ; Limit man to 80 character width
      message-send-mail-partially-limit nil          ; Never split emails
      messages-buffer-max-lines (* 16 1024)          ; From 1024

      kill-ring-max 120                              ; Default is 60
      sentence-end "\\.  ?"                          ; Used only by certain modes.
      ;; scroll-conservatively most-positive-fixnum     ; Always scroll one line at a time
      scroll-preserve-screen-position t              ; Affects Page-up Page-down
      mouse-yank-at-point t                          ; Yank at point, even in X
      lazy-highlight-initial-delay 0.15              ; Seconds to wait before isearch highlights

      ;; choose browser
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (if (eq system-type 'darwin) "open" "firefox")
      frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name)))

      ;; calendar
      calendar-week-start-day 1              ; Start week on Monday
      calendar-date-style 'european          ; Use European calendar
      display-time-world-buffer-name "*World Clock*"

      ;; holidays
      calendar-mark-holidays-flag t
 calendar-holidays nil
 holiday-bahai-holidays nil
 holiday-christian-holidays nil
 holiday-dragon-holidays nil
 holiday-general-holidays nil
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-solar-holidays nil

      )

;; - two spaces is used for delimiters in use-package statements
;; - \n gives correct line count for page
(setq page-delimiter "^ {0,2}\C-l\n")

(setq enable-recursive-minibuffers t)

;; This is very slow, due to Terminus font? -- skangas @ 2019-11-05
;; (add-to-list 'auto-coding-alist '("\\.nfo\\'" . cp437-dos))

;; (setq-default bidi-display-reordering nil)

(setq-default fill-column 80      ;; note to self: use M-q and C-u 78 C-x f
              indent-tabs-mode nil                   ; Always indent using spaces, never tabs
              indicate-empty-lines t                 ; Show empty lines at end of file
              indicate-buffer-boundaries 'left)      ; Show markers indicating buffer limits

(setq holiday-swedish-holidays '((holiday-fixed 1 1 "Nyårsdagen")
                                 (holiday-fixed 1 6 "Trettondedag jul")
                                 (holiday-fixed 5 1 "Första maj")
                                 (holiday-fixed 6 1 "Sveriges nationaldag")
                                 (holiday-fixed 1 25 "Juldagen")
                                 (holiday-fixed 1 26 "Annandag jul")
                                 (holiday-fixed 1 31 "Nyårsafton"))
      calendar-holidays holiday-swedish-holidays)

(setq display-time-world-list '(("America/Los_Angeles" "Seattle")
                                ("America/New_York" "New York")
                                ("America/Toronto" "Toronto")
                                ("Europe/London" "London")
                                ("Europe/Paris" "Paris")
                                ("Europe/Stockholm" "Göteborg")
                                ("Europe/Rome" "Rome")
                                ("Asia/Karachi" "Karachi")
                                ("Asia/Shanghai" "Shanghai")
                                ("Asia/Tokyo" "Tokyo")))

(setq sv-hide-some-holidays t)
(require 'sv-kalender)

(require 'saveplace) ; has to be a require
(setq save-place-file "~/.emacs.d/saveplace") ; keep my ~/ clean
(setq-default save-place t)                   ; activate it for all buffers

(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'uniquify) ;; has to be a require
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; (setq use-dialog-box nil) ;; DON'T DO THIS! Will unfortunately sometimes crash emacs

(add-hook 'before-save-hook 'time-stamp)

;; Zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
\(fn arg char)"
  'interactive)
(global-set-key "\M-z" 'zap-up-to-char)

;; Enable some features
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

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

;; Confirm on exit
(defun confirm-exit-emacs ()
        "ask for confirmation before exiting emacs"
        (interactive)
        (if (yes-or-no-p "Are you sure you want to exit? ")
                (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; Wait for wheezy or install hunspell-sv-se from testing
;; http://packages.debian.org/wheezy/hunspell-sv-se

;; (eval-after-load "ispell"
;;     (progn
;;       (setq ispell-dictionary "swedish"
;; 	    ispell-extra-args '("-a" "-i" "utf-8") ; aspell doesn't understand -i utf-8, hunspell needs it
;; 	    ispell-silently-savep t)))

;; (setq-default ispell-program-name "hunspell")

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

;; FIXME: Move this somewhere else.
(progn
  ;; I can never remember the correct name for this.  So whatever.
  (defalias 'toolbar-mode 'tool-bar-mode))


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

(use-package ace-jump-mode
  :ensure t
  :bind (("C-," . ace-jump-mode)
         ("C-x SPC" . ace-jump-mode-pop-mark)))

(use-package ag
  :ensure t)

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode +1))

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
  (eval-after-load 'auto-dim-other-buffers
    '(diminish 'auto-dim-other-buffers-mode ""))
  (diminish 'abbrev-mode "Ab")
  (diminish 'eldoc-mode " Doc")
  (eval-after-load "anzu"
    '(diminish 'anzu-mode ""))
  (eval-after-load "company"
    '(diminish 'company-mode "Cmp"))
  (eval-after-load 'enh-ruby-mode
    '(diminish 'enh-ruby-mode "Ruby"))
  (eval-after-load 'paredit
    '(diminish 'paredit-mode "PE"))
  (eval-after-load 'minitest
    '(diminish 'minitest-mode "MT"))
  (eval-after-load 'robe
    '(diminish 'robe-mode "Ro"))
  (eval-after-load 'ruby-test-mode
    '(diminish 'ruby-test-mode "RT")))

(use-package dired
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)
              ("," . dired-hide-details-mode)
              ("å" . dired-open-feh)
              ("C-i" . image-dired-here))
  :config
  (setq dired-listing-switches "-lAh"  ; Use human sizes
        dired-dwim-target t            ; Try to guess a default target directory
        dired-isearch-filenames 'dwim) ; Search filenames only
  (setq dired-auto-revert-buffer t)    ; Revert dired buffer on visit
  ;; Toggle showing dot-files using "."
  (define-minor-mode dired-hide-dotfiles-mode
    ""
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

  ;; image-dired
  (setq image-dired-dir "~/.emacs.d/cache/image-dired/")
  (setq image-dired-thumb-width  150
        image-dired-thumb-height 150)
  (defun image-dired-here ()
    "Make a preview buffer for all images in current directory and display it."
    (interactive)
    (image-dired default-directory))

  (defun dired-open-feh ()
    "Make a preview buffer for all images in current directory and display it."
    (interactive)
    (let ((cmd "feh -F -Z -r -z * &" ))
      (message cmd)
      (dired-do-shell-command cmd nil (list (dired-get-file-for-visit)))))

  ;; (defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  ;;   "Run a shell command
  ;; git COMMAND
  ;; ' on the marked files.
  ;; if no files marked, always operate on current line in dired-mode
  ;; "
  ;;   (interactive
  ;;    (let ((files (dired-get-marked-files t current-prefix-arg)))
  ;;      (list
  ;;       ;; Want to give feedback whether this file or marked files are used:
  ;;       (dired-read-shell-command "git command on %s: " current-prefix-arg files)
  ;;       current-prefix-arg
  ;;       files)))
  ;;   (unless (string-match "[?][ \t]\'" command)
  ;;     (setq command (concat command " *")))
  ;;   (setq command (concat "git " command))
  ;;   (dired-do-shell-command command arg file-list)
  ;;   (message command))
  ;; (eval-after-load 'dired '(define-key dired-mode-map "/" 'diredext-exec-git-command-in-shell))

  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1))

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

(use-package eww
  :config
  (setq shr-width 80)
  (defun sk/my-eww-mode-hook ()
    (setq line-spacing 5))
  (add-hook 'eww-mode-hook 'sk/my-eww-mode-hook))

(use-package f
  :ensure t)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(use-package flyspell :ensure nil
  :config
  ;; Non-nil means that flyspell uses M-TAB to correct word.
  (setq flyspell-use-meta-tab nil)
  ;; If non-nil, add correction to abbreviation table.
  (setq flyspell-abbrev-p t)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(use-package google-translate
  :ensure t
  :bind (("C-c t" . google-translate-at-point)
         ("C-c T" . google-translate-query-translate)))

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
             (mode . org-agenda-mode)
             (name . "\*mentor\*")
             (name . "magit:")))
           ("Mentor"
            (filename . "wip/mentor"))
           ("Text Files"
            (or
             (mode . org-mode)
             (mode . text-mode)
             ))
           ("Dired"
            (or
             (mode . dired-mode)))
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
           ("Mail"
            (or
             (mode . message-mode)
             (mode . mail-mode)
             (mode . gnus-group-mode)
             (mode . gnus-summary-mode)
             (mode . gnus-article-mode)
             ))
           ("Emacs Configuration"
            (or (filename . ".emacs.d")
                (filename . ".emacs")))
           ("Emacs Lisp"
            (mode . emacs-lisp-mode))
           ("Configuration"
            (or
             (mode . conf-unix-mode)))
           ("Images"
            (or
             (mode . image-mode)))
           ("IRC"
            (mode . rcirc-mode)))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t))

(use-package ido
  :config
  (ido-mode 0)
  (ido-everywhere 0)

  (setq ido-enable-flex-matching t
        ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-work-directory-list '("~/" "~/org" "~/src")
        ido-case-fold t                 ; Be case-insensitive
        ido-max-directory-size 100000   ; Avoid [Too Big] messages
        ;; display matches vertically
        ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                                " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  (dolist (file-ending '("os" "pyc"))
    (add-to-list 'ido-ignore-files (concat "." file-ending "$")))

  ;; http://whattheemacsd.com/setup-ido.el-02.html
  (defun my-ido-go-straight-home ()
    ;; Go straight home
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
        (interactive)
        (if (looking-back "/")
            (insert "~/")
          (call-interactively 'self-insert-command)))))
  (add-hook 'ido-setup-hook 'my-ido-go-straight-home))

;; ;; Disabled.  It is slow and sometimes broken.
;; (use-package ido-completing-read+
;;   :ensure t
;;   :config
;;   (ido-ubiquitous-mode 1)
;;   (setq ido-cr+-auto-update-blacklist t))

(use-package iedit
  :ensure t)

(use-package ioccur
  :pin "melpa"
  :ensure t)

(use-package isearch
  :config
  (setq isearch-allow-scroll t))

(use-package midnight ; close inactive buffers
  :config
  (midnight-delay-set 'midnight-delay "06:00")
  (timer-activate midnight-timer))

(use-package multiple-cursors
  :ensure t)

(use-package openwith                   ; open files using external helpers
  :ensure t
  :pin "melpa"
  :config
  (openwith-mode t)
  (setq my-video-types
        (concat (regexp-opt '(".asf" ".avi" ".f4v"
                              ".flv" ".m4a" ".m4v"
                              ".mkv" ".mov" ".mp4"
                              ".m2ts" ".mpeg" ".mpg"
                              ".ogv" ".wmv" ".webm")) "\\'"))

  (setq openwith-associations
        `((,my-video-types "mpv --cache=50000" (file))
          ("\\(?:\\.img\\|\\.iso\\)\\'" "mpv" ("dvd://" "-dvd-device" file))
          ("\\.azw3\\'" "calibre" (file))
          ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
          ;; ("\\.mp3\\'" "mplayer" (file))
          ("\\.pdf\\'" "evince" (file))
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

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-save-file "~/.emacs.d/cache/recentf"
        recentf-exclude '("^/home/skangas/org/.*"
                          "^/home/skangas/.emacs.bmk$"
                          "^/Users/skangas/org/.*")))

(use-package seq
  :ensure t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package tramp
  :config
  ;; (setq tramp-default-method "ssh")

  ;; (set-default 'tramp-default-proxies-alist
  ;;              '((".*" "\\`root\\'" "/ssh:%h:")))

  ;; don't backup files edited in tramp using sudo or su -- we don't want to
  ;; spread secret root files around.
  (setq backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not
                (let ((method (file-remote-p name 'method)))
                  (when (stringp method)
                    (member method '("su" "sudo")))))))))

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

(use-package winner
  :bind (("<C-s-left>" . winner-undo)
         ("<C-s-right>" . winner-redo))
  :config
  (setq winner-dont-bind-my-keys t) ; default bindings conflict with org-mode
  (winner-mode 1))

(use-package winum
  ;; Replaces window-numbering.el
  :ensure t
  :pin "melpa-stable"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; image-mode

;; https://emacs.stackexchange.com/questions/2433/shrink-zoom-scale-images-in-image-mode
(defun image-transform-fit-to-window ()
  "Resize the image to fit the width or height based on the image and
window ratios.  Imagemagick is required to run this function."
  (interactive)
  (let* ( (img-size (image-display-size (image-get-display-property) t))
          (img-width (car img-size))
          (img-height (cdr img-size))
          (img-h/w-ratio (/ (float img-height) (float img-width)))
          (win-width (- (nth 2 (window-inside-pixel-edges))
                        (nth 0 (window-inside-pixel-edges))))
          (win-height (- (nth 3 (window-inside-pixel-edges))
                         (nth 1 (window-inside-pixel-edges))))
          (win-h/w-ratio (/ (float win-height) (float win-width))))
    ;; Fit image by width if the h/w ratio of window is > h/w ratio of the image
    (if (> win-h/w-ratio img-h/w-ratio)
        (image-transform-fit-to-width)
      ;; Else fit by height
      (image-transform-fit-to-height))))

(defvar sk/image-mode-resized t)
(make-variable-buffer-local 'sk/image-mode-resized)

(defun sk/image-mode-resize-maybe-hook ()
  (when sk/image-mode-resized
    (image-transform-fit-to-window)))
(add-hook 'image-mode-hook 'sk/image-mode-resize-maybe-hook)

(defun sk/image-mode-toggle-resized ()
  (interactive)
  (if sk/image-mode-resized
      nil
    (image-transform-fit-to-window))
  (setq sk/image-mode-resized (not sk/image-mode-resized)))
(eval-after-load 'image-mode
  '(progn
     (define-key image-mode-map " " 'image-next-file)
     (define-key image-mode-map "V" 'sk/image-mode-toggle-resized)))

(setq image-animate-loop t)
(add-hook 'image-mode-hook 'sk/image-mode-resize-maybe-hook)

(provide 'init-general)
