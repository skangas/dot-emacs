;;; init-general.el --- general customizations
;;; Commentary:
;;; Code:

;;; General settings
(require 'ansi-color)


;;;; Various configuration settings
;; FIXME: add visual line mode to all modes where it makes sense

(defmacro run-if-fboundp (arg)
  (if (fboundp (car arg)) arg))
(unless (eq window-system 'ns)
  (run-if-fboundp (menu-bar-mode -1)))
(run-if-fboundp (scroll-bar-mode -1))
(run-if-fboundp (tool-bar-mode -1))

(run-if-fboundp (mouse-wheel-mode 1))
(run-if-fboundp (column-number-mode 1))
(run-if-fboundp (line-number-mode 1))
(run-if-fboundp (auto-image-file-mode 1))
(run-if-fboundp (display-time-mode 1))
(save-place-mode 1)
(savehist-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)

;; * Font Lock mode, Auto Compression mode, and File Name Shadow Mode
;;   are enabled by default.

(setq scroll-conservatively 500)
(setq scroll-step 0)
(setq scroll-preserve-screen-position nil)

(setq scroll-margin 2)

(setq user-full-name "Stefan Kangas"
      user-mail-address "stefankangas@gmail.com"
      require-final-newline t               ; Make sure text files end in a newline
      message-send-mail-partially-limit nil ; Never split emails
      kill-ring-max (* kill-ring-max 4)
      undo-limit (* undo-limit 4)
      undo-strong-limit (* undo-strong-limit 4)
      undo-outer-limit (* undo-outer-limit 4)
      ;; scroll-conservatively most-positive-fixnum     ; Always scroll one line at a time
      scroll-preserve-screen-position t ; Affects Page-up Page-down
      mouse-yank-at-point t             ; Yank at point, even in X
      lazy-highlight-initial-delay 0.15 ; Seconds to wait before isearch highlights
      save-interprogram-paste-before-kill t
      apropos-do-all t

      ;; choose browser
      browse-url-browser-function #'browse-url-generic
      browse-url-generic-program (if (eq system-type 'darwin) "open" "firefox")

      frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name))))

(setq show-paren-context-when-offscreen 'child-frame)
(setq proced-enable-color-flag t)

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

(setq sv-hide-some-holidays t)
(require 'sv-kalender)

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
  "Change cursor color according to some minor modes."
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

;; auto-save-visited-mode
(defun my/auto-save-visited-predicate ()
  (and (eq major-mode 'org-mode)
       (string-match "^/home/skangas/org/" buffer-file-name)))
(setq auto-save-visited-predicate #'my/auto-save-visited-predicate)
(auto-save-visited-mode 1)


;;;; packages

(use-package emacs
  ;; This is a bit broken.
  :hook (term-mode . (lambda () (hl-line-mode 'toggle))))

(use-package abbrev :ensure nil         ; built-in
  :defer 5
  :diminish "Ab"
  :hook (erc-mode text-mode)
  :custom
  (abbrev-file-name "~/org/.abbrev_defs")
  (save-abbrevs t)
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package avy
  :bind (("C-å" . avy-goto-char-timer)
         ("C-Å" . avy-goto-char)
         ("C-x SPC" . avy-mark)
         ("M-g f" . avy-goto-line)))

(use-package ag
  :defer t)

(use-package async
  :after dired
  :config
  (dired-async-mode 1))

(use-package beacon
  :custom
  (beacon-blink-delay 0.1)
  (beacon-blink-duration 0.1)
  (beacon-color 0.2)
  :diminish beacon-mode
  :init (beacon-mode 1)
  :config
  (setq beacon-dont-blink-commands
        (cl-delete-duplicates
         (append beacon-dont-blink-commands
                 '(beginning-of-buffer
                   end-of-buffer
                   dired-find-file
                   magit-previous-line
                   magit-next-line
                   paredit-backward
                   paredit-forward
                   )))))

(use-package centered-cursor-mode
  :disabled t
  :config
  ;; center cursor in info-mode
  (defun my-info-mode-hook-center-cursor ()
    (centered-cursor-mode))
  (setq Info-mode-hook 'my-info-mode-hook-center-cursor))

(use-package auto-dim-other-buffers
  ;; :pin "melpa"
  :disabled t
  :diminish
  :config
  (auto-dim-other-buffers-mode 1))

(use-package boxquote
  :defer t)

;; (use-package centered-window
;;   :pin "melpa"
;;   :defer t)

(use-package comint :ensure nil       ; built-in
  ;; FIXME: :defer should not be needed here
  :defer t
  :hook (comint-mode . (lambda () (setq-local scroll-margin 0))))

(use-package company
  :pin "gnu"
  :defer 5
  :diminish "comp"
  :custom
  ;; Using company mode in eshell and shell-mode causes problems
  (company-global-modes '(not eshell-mode shell-mode))
  :config
  (global-company-mode 1))

(use-package counsel
  :pin "gnu"
  :bind
  (nil
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . my/counsel-yank-or-yank-pop)

   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-c t" . counsel-load-theme)
   ("C-h l" . counsel-find-library)

   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("<f2> j" . counsel-set-variable)

   ;; ("C-x b" . ivy-switch-buffer)
   ;; ("C-c v" . ivy-push-view)
   ;; ("C-c V" . ivy-pop-view)

   ;; ;; Ivy-based interface to shell and system tools
   ;; ("C-c c" . counsel-compile)
   ;; ("C-c g" . counsel-git)
   ;; ("C-c j" . counsel-git-grep)
   ;; ("C-c L" . counsel-git-log)
   ;; ("C-c k" . counsel-rg)
   ;; ("C-c m" . counsel-linux-app)
   ;; ("C-c n" . counsel-fzf)
   ;; ("C-x l" . counsel-locate)
   ;; ("C-c J" . counsel-file-jump)
   ;; ("C-S-o" . counsel-rhythmbox)
   ;; ("C-c w" . counsel-wmctrl)

   ;; ("C-c b" . counsel-bookmark)
   ;; ("C-c d" . counsel-descbinds)
   ;; ("C-c g" . counsel-git)
   ;; ("C-c o" . counsel-outline)
   ;; ("C-c F" . counsel-org-file)
   )
  :preface
  (defun my/counsel-yank-or-yank-pop (&optional arg)
    "Call `consult-yank'. If called after a yank, call `yank-pop' instead."
    (interactive "*p")
    (if (eq last-command 'yank)
        (yank-pop arg)
      (counsel-yank-pop))))

(use-package dash
  :defer t)

(use-package diff-hl
  :hook (dired-mode-hook . diff-hl-dired-mode))

(use-package dired :ensure nil          ; built-in
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode)
              ("," . dired-hide-details-mode)
              ("å" . dired-sk/open-media-dwim)
              ("E" . dired-do-eww)
              ("C-i" . image-dired-here))
  :custom
  (dired-dwim-target t)                ; Try to guess a default target directory
  (dired-isearch-filenames 'dwim)      ; Search filenames only
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-make-directory-clickable t)    ; 29.1
  (dired-hide-details-hide-symlink-targets nil)
  (dired-omit-verbose nil)
  (dired-omit-files
   (concat dired-omit-files
           (rx (or ".pytest_cache" ".ruff_cache") eos)))
  :config
  ;; This actually binds `mouse-1'.
  (define-key dired-mode-map [mouse-2] #'dired-mouse-find-file)

  (if (eq system-type 'darwin)
      (setq dired-listing-switches "-lAFh")
    (setq dired-listing-switches "-lAFh --group-directories-first"))

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  (define-minor-mode dired-hide-dotfiles-mode
    "Toggle showing dot-files."
    :lighter " Hide"
    :init-value nil
    (if (not (derived-mode-p 'dired-mode))
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
      (dired-do-shell-command cmd nil (list file)))))

(use-package dired-collapse
  :hook dired-mode)

(use-package diredfl
  :hook dired-mode)

(use-package dired-aux :ensure nil      ; built-in
  :defer t
  :config
  (push `(,sk/video-types "mpv")
        dired-guess-shell-alist-default)
  (setq dired-create-destination-dirs 'ask))

(use-package docker-compose-mode
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package eglot :ensure nil          ; built-in
  :hook (python-mode . eglot-ensure))

(use-package eldoc :ensure nil          ; built-in
  :diminish)

(use-package engine-mode
  :config
  (engine-mode t)
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d"))

(use-package epa-file :ensure nil       ; built-in
  :defer t
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

(use-package erc :ensure nil            ; built-in
  :hook (erc-mode . abbrev-mode))

(use-package eshell :ensure nil         ; built-in
  :hook (eshell-mode . (lambda () (setq-local scroll-margin 0)))
  :custom
  (eshell-visual-subcommands '(("git" "log" "diff" "show" "tag"))))

(use-package eww :ensure nil            ; built-in
  :defer t
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

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package flyspell :ensure nil       ; built-in
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-welcome-flag nil)
  ;; Non-nil means that flyspell uses M-TAB to correct word.
  (flyspell-use-meta-tab nil)
  ;; If non-nil, add correction to abbreviation table.
  (flyspell-abbrev-p t))

(use-package embark                     ; put after flyspell
  :pin "gnu"
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
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

(use-package grep :ensure nil           ; built-in
  :defer t
  :config

  ;; TODO: Should this be in `python-mode'?
  (add-to-list 'grep-files-aliases '("py" . "*.py"))
  (dolist (dir '(".tox" ".venv" ".mypy_cache" ".ruff_cache"))
    (add-to-list 'grep-find-ignored-directories dir))

  (defun sk/compilation-finish-flush-lines (buf _)
    "Flush irrelevant lines in grep buffers."
    (with-current-buffer buf
      (save-excursion
        (let ((buffer-read-only nil))
          (goto-char (point-min))
          (flush-lines (rx bol "." (? "/lisp") "/"
                           (or
                            "etc/DOC"
                            "ldefs-boot.el"
                            "loaddefs.el"
                            "calc/calc-loaddefs.el"
                            "calendar/diary-loaddefs.el"
                            "calendar/holiday-loaddefs.el"
                            "emacs-lisp/cl-loaddefs.el"
                            "erc/erc-loaddefs.el"
                            "mh-e/mh-loaddefs.el"
                            "ibuffer-loaddefs.el"
                            "net/tramp-loaddefs.el"
                            "org/org-loaddefs.el"
                            "textmodes/reftex-loaddefs.el"
                            "textmodes/texinfo-loaddefs.el"
                            (: "test/manual/etags/" (+ (any "a-z")) "-src/")
                            )))))))

  (defun sk/grep-mode-hook ()
    (setq-local compilation-finish-functions #'sk/compilation-finish-flush-lines))

  (add-hook 'grep-setup-hook 'sk/grep-mode-hook))

;; (use-package google-translate
;;   :bind (("C-c t" . google-translate-at-point)
;;          ("C-c T" . google-translate-query-translate)))

;; (use-package guess-language-mode
;;   :config
;;   (setq guess-language-languages '(en sv)))

(use-package ibuffer :ensure nil        ; built-in
  :defer t
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-expert t)
  (ibuffer-filter-group-name-face '(:inherit (success bold)))
  (ibuffer-saved-filter-groups
   '(("default"
      ("Work"
       (or
        (mode . org-agenda-mode)
        (name . "org/.+")))
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
      ("init.el"
       (or (filename . "\\.emacs\\.d")
           (filename . "\\.emacs")))
      ("Mentor"
       (filename . "wip/mentor"))
      ("emacs.git"
       (filename . "/wip/emacs.+"))
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
  :hook (ibuffer-mode .
                      (lambda ()
                        (ibuffer-auto-mode 1)
                        (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  ;; Redefine size column to display human readable size
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

(use-package ido :ensure nil            ; built-in
  :defer t
  :config
  ;; (ido-everywhere -1)
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

(use-package ido-completing-read+
  :disabled t                           ; It is slow and sometimes broken.
  :after ido
  :config
  (ido-ubiquitous-mode 1)
  (setq ido-cr+-auto-update-blacklist t))

(use-package iedit
  :bind ("C-M-y" . iedit-mode))

(use-package image-dired :ensure nil    ; built-in
  :defer t
  :config
  (setq image-dired-dir "~/.emacs.d/cache/image-dired/")
  (setq image-dired-thumb-width  150
        image-dired-thumb-height 150)

  (defun image-dired-here ()
    "Make a preview buffer for all images in current directory and display it."
    (interactive)
    (image-dired default-directory)))

(use-package image-mode :ensure nil     ; built-in
  :defer t
  :bind (:map image-mode-map
              ("SPC" . #'image-next-file)
              ("V" . #'sk/image-mode-toggle-resized)))

(use-package ioccur
  :pin "gnu"
  :defer t)

(use-package ispell :ensure nil         ; built-in
  :defer t
  :config
  (when (equal system-type 'darwin)
    (setenv "LANG" "en_US.UTF-8"))
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-a" "-i" "utf-8"))
  (setq ispell-dictionary "en_US,sv_SE,es_ES")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,sv_SE,es_ES")
  (setq ispell-silently-savep t)
  (setq flyspell-use-global-abbrev-table-p t))

(use-package man :ensure nil            ; built-in
  :bind (:map Man-mode-map
              ("w"   . my/man-copy-name-as-kill)
              ("M-w" . my/kill-ring-save-without-whitespace))
  :init
  (setopt Man-switches (if '(eq system-type darwin)
                           ;; The below flags aren't supported in BSD.
                           ""
                         "--no-hyphenation --no-justification"))
  (defun my/man-copy-name-as-kill ()
    (interactive nil Man-mode)
    (when-let ((str
                (save-excursion
                  (goto-char (point-min))
                  (and (looking-at (rx bol )))))))
    (setq str
          (if (string-match " " Man-arguments)
              (let ((args (string-split Man-arguments " ")))
                (apply #'format "%s(%s)" (reverse args)))
            Man-arguments))
    (kill-new str)
    (message str))

  (defun my/kill-ring-save-without-whitespace (beg end &optional region arg)
    "Like `kill-ring-save', but filter all spaces.
With prefix ARG, don't filter anything."
    (interactive (list (mark) (point) 'region current-prefix-arg))
    (let ((filter-buffer-substring-function
           (if (not arg)
               (lambda (beg end &optional delete)
                 (replace-regexp-in-string
                  "‐ " ""
                  (replace-regexp-in-string
                   (rx (+ space)) " "
                   (buffer-substring beg end))))
             filter-buffer-substring-function)))
      (kill-ring-save beg end region))))

(use-package marginalia
  :defer 3
  :pin "gnu"
  :config
  (marginalia-mode 1))

(use-package midnight :ensure nil       ; built-in
  :defer 30
  :custom
  (clean-buffer-list-delay-general 7)   ; default is 3 days
  :config
  (midnight-mode 1)
  (midnight-delay-set 'midnight-delay "06:00")
  (when (fboundp 'native-compile-prune-cache)
    (add-to-list 'midnight-hook 'native-compile-prune-cache)))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :hook ((markdown-mode . orgtbl-mode)
         (markdown-mode . visual-line-mode))
  :custom-face
  (markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
  (markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
  (markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
  (markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2)))))

(use-package multiple-cursors
  :defer t)

(use-package occur :ensure nil
  :bind (:map occur-mode-map
              ("d" . occur-mode-display-occurrence)
              ("n" . next-logical-line)
              ("p" . previous-logical-line)))

(use-package openwith                   ; open files using external helpers
  :defer 10
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

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :bind (:map nov-mode-map
              ("u" . #'nov-goto-toc)))

(use-package orderless
  :defer 10
  :pin "gnu"
  :custom (completion-styles '(orderless basic)))

;; (use-package powerline
;;   :config
;;   (powerline-default-theme))

(use-package rainbow-delimiters
  :hook prog-mode)

(use-package recentf :ensure nil        ; built-in
  :defer 10
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file "~/.emacs.d/cache/recentf")
  (recentf-exclude `(,(rx bos "/" (or "home" "Users") "/skangas/"
                          (or (seq "org/" (* any))
                              (seq ".emacs.bmk" eos))))))

(use-package sql-upcase :ensure nil  ; in "lisp-contrib"
  :commands sql-upcase-mode sql-upcase-region
  :hook (sql-mode sql-interactive-mode))

(use-package sql-indent
  :config
  (setq-default sqlind-basic-offset 4))

(use-package tab-bar :ensure nil        ; built-in
  :bind (("C-x t n" . tab-next)
         ("C-x t p" . tab-previous)
         ("C-x t o" . tab-duplicate)
         ("C-x t O" . project-other-tab-command)))

(use-package time :ensure nil           ; built-in
  :defer t
  :custom
  (world-clock-time-format "%R %z %4Z  %A %d %B")
  (world-clock-buffer-name "*world-clock*")
  (world-clock-list '(("America/Los_Angeles" "Seattle")
                      ("America/Chicago" "Chicago")
                      ("America/New_York" "New York")
                      ("UTC" "UTC")
                      ("Europe/London" "London")
                      ("Europe/Stockholm" "Stockholm")
                      ("Europe/Rome" "Rome")
                      ("Asia/Karachi" "Karachi")
                      ("Asia/Shanghai" "Shanghai")
                      ("Asia/Tokyo" "Tokyo")
                      ("Australia/Sydney" "Sydney"))))

(use-package tramp :ensure nil          ; built-in
  :defer t
  :config
  ;; don't backup any remote files:
  ;; (info "(tramp) Auto-save File Lock and Backup")
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(use-package undo-tree
  :defer t
  :pin "gnu")

(use-package uniquify :ensure nil
  :demand t)                            ; has to be a require

(use-package visual-line-mode :ensure nil ; built-in
  :hook (Custom-mode))

(use-package wgrep
  :after grep
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c". wgrep-finish-edit)))

(use-package writegood-mode
  :defer t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-add-major-mode-key-based-replacements 'enh-ruby-mode
    "C-c r !" "Run rails"
    "C-c r T" "Go to test/toggle"
    "C-c r t" "Go to test/search"))

(use-package winner :ensure nil         ; built-in
  :bind (("<C-s-left>" . winner-undo)
         ("<C-s-right>" . winner-redo))
  :init
  (setq winner-dont-bind-my-keys t)    ; default bindings conflict with org-mode
  (winner-mode 1))

(use-package winum
  ;; Replaces window-numbering.el
  :pin "melpa"
  :init (winum-mode)
  :custom (winum-scope 'frame-local)
  :bind ( :map winum-keymap
          ("M-1" . winum-select-window-1)
          ("M-2" . winum-select-window-2)
          ("M-3" . winum-select-window-3)
          ("M-4" . winum-select-window-4)
          ("M-5" . winum-select-window-5)
          ("M-6" . winum-select-window-6)
          ("M-7" . winum-select-window-7)
          ("M-8" . winum-select-window-8)
          ("M-9" . winum-select-window-9)))

(use-package yasnippet
  :defer 20
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-general)

;;; init-general.el ends here
