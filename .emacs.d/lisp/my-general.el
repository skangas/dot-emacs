;;; General settings

(setq split-width-threshold 160)
(setq split-height-threshold 0)

(require 'ffap)
(require 'ansi-color)

;; Change all yes or no prompt to y or n prompts:
(fset 'yes-or-no-p 'y-or-n-p)

;; Various configuration settings

   ;; * Font Lock mode, Auto Compression mode, and File Name Shadow Mode
   ;;   are enabled by default.

(defmacro run-if-fboundp (arg)
  (if (fboundp (car arg)) arg))

(when (>= emacs-major-version 24)
  (require 'zenburn-theme))

;; FIXME: add visual line mode to all modes where it makes sense

(run-if-fboundp (menu-bar-mode -1))        ; No menu
(run-if-fboundp (scroll-bar-mode -1))      ; No scrollbar
(run-if-fboundp (tool-bar-mode -1))        ; No toolbar
(run-if-fboundp (mwheel-install))          ; Enable mousewheel

(run-if-fboundp (global-font-lock-mode t)) ; Syntax hi-lighting
(run-if-fboundp (column-number-mode 1))    ; Show column number
(run-if-fboundp (line-number-mode 1))      ; Show line number

(run-if-fboundp (auto-image-file-mode 1))  ; View images in emacs
(run-if-fboundp (auto-compression-mode 1)) ; Automatically read/write compressed files

(setq user-full-name "Stefan Kangas")

(setq inhibit-startup-message t)                     ; No startup message
(setq frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name))))

(setq scroll-conservatively most-positive-fixnum)    ; Always scroll one line at a time
(setq scroll-preserve-screen-position t)             ; Affects Page-up Page-down
(setq visible-bell t)                                ; No audible bell
(setq mouse-yank-at-point t)                         ; Yank at point, even in X
(setq lazy-highlight-initial-delay 0.1)              ; Seconds to wait before isearch highlights

(setq-default fill-column 80)  ;; note to self: use M-q and C-u 78 C-x f
(setq-default indent-tabs-mode nil)                  ; Always indent using spaces, never tabs

(setq display-time-24hr-format t)                    ; Show 24hr clock when it's shown
(setq bookmark-save-flag 1)                          ; Save bookmarks immediately when added
(setq require-final-newline t)                       ; Make sure text files end in a newline
(setq Man-width 80)                                  ; Limit man to 80 character width
(setq message-send-mail-partially-limit nil)         ; Never split emails
(setq messages-buffer-max-lines (* 16 1024))         ; From 1024

;; FIXME: These are obsolete now...
(setq default-indicate-empty-lines t)                ; Show empty lines at end of file
(setq default-indicate-buffer-boundaries 'left)      ; Show markers indicating buffer limits

;; (setq use-dialog-box nil) ;; DON'T DO THIS! Will unfortunately sometimes crash emacs

(add-hook 'before-save-hook 'time-stamp)

;; Enable some features
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Used only by certain modes.
(setq sentence-end "\\.  ?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup files
(setq version-control t         ; use versioned backups
      kept-old-versions 255
      kept-new-versions 1024
      delete-old-versions t
      backup-by-copying t       ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/cache/saves")))    ; don't litter my fs tree

;; Delete old and big backup files that just wastes space
(let ((bak-dir (expand-file-name "~/.emacs.d/cache/saves")))
  (when (and (file-exists-p bak-dir)
             (file-directory-p bak-dir))
    (start-process (concat "delete old backup files in " bak-dir)
                   "*Messages*" "find" bak-dir "-size" "+1M" "-mtime" "+90" "-delete")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; midnight-mode - close inactive buffers
(require 'midnight)
(midnight-delay-set 'midnight-delay "06:00")
(timer-activate midnight-timer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido-mode

(require 'ido)
(ido-mode t)
(ido-everywhere 1)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"

      ido-default-file-method 'selected-window
      ido-default-buffer-method 'selected-window

      ido-work-directory-list '("~/" "~/org" "~/src")
      ido-case-fold t                   ; Be case-insensitive
      ido-max-directory-size 100000     ; Avoid [Too Big] messages
      ;; display matches vertically
      ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                              " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))) 

(dolist (file-ending '("os" "pyc"))
  (add-to-list 'ido-ignore-files (concat "." file-ending "$")))


;;;; WORKAROUND FOR GNUS BUG
;;;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-01/msg00613.html
(add-hook 'ido-before-fallback-functions
        (lambda (fn)
            (and (eq fn 'read-file-name)
                 (> (length ido-text) 0)
                 (boundp 'initial)
                 (setq initial nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window numbering

(require 'window-numbering)
(window-numbering-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor
;; cursor-chg
;(require 'cursor-chg)  ; Load the library
;; (toggle-cursor-type-when-idle 0) ; Turn on cursor change when Emacs is idle
;; (change-cursor-mode 0) ; Turn on change for overwrite, read-only, and input mode

;; (setq curchg-default-cursor-color "white") ;; FIXME: add flag to stop cursor-chg
                                           ;; from changing color, or make it
                                           ;; aware of buffer-read-only and
                                           ;; overwrite-mode

;; Change cursor color depending on context (EmacsNiftyTricks)
(setq hcz-set-cursor-color-color "")
(setq hcz-set-cursor-color-buffer "")
(defun hcz-set-cursor-color-according-to-mode ()
  "change cursor color according to some minor modes."
  ;; set-cursor-color is somewhat costly, so we only call it when needed:
  (let ((color
         (if buffer-read-only "#8888FF"
           (if overwrite-mode "#000000"
             "#FF0000"))))
    (unless (and
             (string= color hcz-set-cursor-color-color)
             (string= (buffer-name) hcz-set-cursor-color-buffer))
      (set-cursor-color (setq hcz-set-cursor-color-color color))
      (setq hcz-set-cursor-color-buffer (buffer-name)))))
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode)

;; center cursor in info-mode
(when (and (require 'info)
           (require 'centered-cursor-mode))
  (defun my-info-mode-hook-center-cursor ()
    (centered-cursor-mode))
  (setq Info-mode-hook 'my-info-mode-hook-center-cursor))

;; Save position in file
(require 'saveplace) ; has to be a require
(setq save-place-file "~/.emacs.d/saveplace") ;; keep my ~/ clean
(setq-default save-place t)                   ;; activate it for all buffers

;; Confirm on exit
(defun confirm-exit-emacs ()
        "ask for confirmation before exiting emacs"
        (interactive)
        (if (yes-or-no-p "Are you sure you want to exit? ")
                (save-buffers-kill-emacs)))
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-c" 'confirm-exit-emacs)

;; Show paren mode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Spell checking
(setq flyspell-use-meta-tab nil)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

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
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
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

;;; abbreviate mode names
(when (require 'diminish nil 'noerror)
  (after 'abbrev
    '(diminish 'abbrev-mode "Ab"))
  (after 'company
    '(diminish 'company-mode "Cmp"))
  (after 'paredit
    '(diminish 'paredit-mode "ParEd"))
  (after 'yasnippet
    '(diminish 'yas/minor-mode "Y")))
(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "el")))

;;; Recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-save-file "~/.emacs.d/cache/recentf")

;;; openwith.el -- open files using external helpers
(require 'openwith)
(openwith-mode t)
(setq my-video-types '(".asf" ".avi" ".f4v"
                       ".flv" ".m4a" ".m4v"
                       ".mkv" ".mov" ".mp4"
                       ".mpeg" ".mpg" ".ogv"
                       ".wmv"))
(setq my-video-types-regexp (regexp-opt my-video-types))

(setq openwith-associations
      (let ((video-types (concat my-video-types-regexp "\\'")))
        `((,video-types "mplayer" ("-idx" file))
          ("\\(?:\\.img\\|\\.iso\\)\\'" "mplayer" ("dvd://" "-dvd-device" file))
          ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))
          ;; ("\\.mp3\\'" "mplayer" (file))
          ;; ("\\.pdf\\'" "evince" (file))
          )))
;;; FIXME: Do not do this for messages
;;; ("\\.odt\\'" "libreoffice" (file))

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
(ad-activate 'abort-if-file-too-large) 

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 10 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    ; (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

;;; ediff
(setq ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))

;;; EPA
(require 'epa-file)
(epa-file-enable)
;(setq epa-armor t)

;; Disable gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

;;; moved here from buffer.el

(setq ibuffer-saved-filter-groups
      '(("default"
         ("Emacs Configuration"
          (or (filename . ".emacs.d")))
         ("Org"
          (mode . org-mode))  
         ("mentor"
          (filename . "wip/mentor"))
         ("Mail"
          (or
           (mode . message-mode)
           (mode . mail-mode)
           (mode . gnus-group-mode)
           (mode . gnus-summary-mode)
           (mode . gnus-article-mode)
           ))
         ("Magit"
          (name . "\*magit:"))
         ("Emacs Lisp"
          (mode . emacs-lisp-mode))
         ("Programming"
          (or
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
         ("Dired"
          (or
           (mode . dired-mode)))
         ("IRC"
          (mode . rcirc-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)

;; Unique buffer names
(require 'uniquify) ;; has to be a require
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; choose browser
(setq browse-url-generic-program "firefox")
(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (w3m-browse-url url)))
;;(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-browser-function 'choose-browser)

;; winner-mode
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode
(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
(winner-mode +1) ;; turn on the global minor mode

(provide 'my-general)

;; my-general.el ends here

