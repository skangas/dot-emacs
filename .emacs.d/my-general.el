;;; General settings

(require 'cl)
(require 'ffap)
(require 'ansi-color)

(require 'sunrise-commander) ; sunrise commander

;; various stuff 
(setq message-log-max 1024) ;; do this first
(setq max-specpdl-size 15600)
(setq max-lisp-eval-depth 9000)

;; Change all yes or no prompt to y or n prompts:
(fset 'yes-or-no-p 'y-or-n-p)

;; Various configuration settings

   ;; * Font Lock mode, Auto Compression mode, and File Name Shadow Mode
   ;;   are enabled by default.

(auto-compression-mode 1)                            ; Automatically read/write compressed files
(auto-image-file-mode 1)                             ; View images in emacs
(column-number-mode 1)                               ; Put column number into modeline
;; (global-visual-line-mode 1)                          ; Wrap sanely
;; FIXME: add visual line mode to all modes where it makes sense


(setq user-full-name "Stefan Kangas")

(setq frame-title-format '((buffer-file-name "%f" "%b")
                           " -- %F"
                           (:eval (format " [%s]" mode-name))))

(global-font-lock-mode t)                            ; Syntax hi-lighting
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ; No menu
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; No scrollbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ; No toolbar
(if (fboundp 'mwheel-install) (mwheel-install))      ; Enable mousewheel
(if (fboundp 'column-number-mode) (column-number-mode -1)) ; No column number
(if (fboundp 'line-number-mode) (line-number-mode -1))     ; No line number
(setq bookmark-save-flag 1)                          ; Save bookmarks immediately when added
(setq default-indicate-buffer-boundaries 'left)      ; Show markers indicating buffer limits
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq default-indicate-empty-lines t)                ; Show empty lines at end of file
(setq inhibit-startup-message t)                     ; No startup message
(setq require-final-newline t)                       ; Make sure text files end in a newline
(setq scroll-conservatively most-positive-fixnum)    ; Always scroll one line at a time
(setq scroll-preserve-screen-position t)             ; Affects Page-up Page-down
(setq cua-enable-cua-keys nil)                       ; No cua-keys
(setq visible-bell t)                                ; No audible bell
(setq-default fill-column 80)  ;; note to self: use M-q and C-u 78 C-x f
(setq-default indent-tabs-mode nil)                  ; Always indent using spaces, never tabs
(setq mouse-yank-at-point t)                         ; Yank to cursor, even in X
(setq fortune-file "~/dokument/quotes")              ; Why do I set this? Nvm, I guess it doesn't hurt...
;; (setq use-dialog-box nil) ;; DON'T DO THIS! Will unfortunately sometimes crash emacs
(when window-system (global-unset-key "\C-z")) ; Disable keyboard iconfying
(setq Man-width 80)                                  ; Limit man to 80 character width
(setq display-time-24hr-format t)                    ; Show 24hr clock when it's shown

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
      backup-directory-alist
      '(("." . "~/.emacs.d/cache/saves")))    ; don't litter my fs tree

;; Delete old and big backup files that just wastes space
(let ((bak-dir (expand-file-name "~/.emacs.d/cache/saves")))
  (when (and (file-exists-p bak-dir)
             (file-directory-p bak-dir))
    (start-process (concat "delete old backup files in " bak-dir)
                   "*Messages*" "find" bak-dir "-size" "+1M" "-mtime" "+90" "-delete")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IsearchOtherEnd - Search restarts at top of buffer if it hits the bottom
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))

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

;; display matches vertically
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
      ido-enable-flex-matching t
      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
        ;; ido-ignore-buffers
      ;; '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
        ;; "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
      ido-work-directory-list '("~/" "~/org" "~/src")
      ido-case-fold t) ; be case-insensitive

(add-to-list 'ido-ignore-files ".os$")

;; open recent files using ido
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-save-file "~/.emacs.d/cache/recentf")
(defun my-ido-recentf-open ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'my-ido-recentf-open)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window numbering

(require 'window-numbering)
(window-numbering-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cua-mode
(cua-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor
;; (blink-cursor-mode 0) ; stop cursor from blinking
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

(autoload 'insert-x-resources "pjb-xresources"
  "Insert current theme as XResources in current buffer" t)

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

(show-paren-mode 1)
(setq show-paren-delay 0)

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (if (not (minibuffer-prompt))
      (let ((matching-text nil))
        ;; Only call `blink-matching-open' if the character before point
        ;; is a close parentheses type character. Otherwise, there's not
        ;; really any point, and `blink-matching-open' would just echo
        ;; "Mismatched parentheses", which gets really annoying.
        (if (char-equal (char-syntax (char-before (point))) ?\))
            (setq matching-text (blink-matching-open)))
        (if (not (null matching-text))
            (message matching-text)))))
    
;; Spell checking

(setq flyspell-use-meta-tab nil)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; center cursor in info-mode
(when (and (require 'info)
           (require 'centered-cursor-mode))
  (defun my-info-mode-hook-center-cursor ()
    (centered-cursor-mode))
  (setq Info-mode-hook 'my-info-mode-hook-center-cursor))

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

;; occur-mode

(defun my-occur-mode-customizations ()
  (define-key occur-mode-map (kbd "d") 'occur-mode-display-occurrence)
  (define-key occur-mode-map (kbd "n") 'next-logical-line)
  (define-key occur-mode-map (kbd "p") 'previous-logical-line))
(add-hook 'occur-mode-hook 'my-occur-mode-customizations)

;; time-stamp
(add-hook 'before-save-hook 'time-stamp)

;; openwith

;; (require 'openwith)

;; (setq openwith-confirm-invocation nil)

;; (defvar my-video-types)
;; (setq my-video-types-regexp (regexp-opt '(".mpg" ".mpeg" ".avi" ".ogv" ".wmv" ".asf" ".flv" ".mov" ".mkv" ".m4a" ".mp4")))

;; (setq openwith-associations
;;       (let ((video-types (concat my-video-types-regexp "\\'")))
;;         `((,video-types "mplayer" ("-idx" file))
;;           ("\\.img\\'" "mplayer" ("dvd://" "-dvd-device" file))
;;           ("\\.mp3\\'" "mplayer" (file))
;;           ("\\.pdf\\'" "evince" (file)))))
;;           ;; ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))))

;;; nisse

(setq lazy-highlight-initial-delay 0.1)

;;; abbreviate mode names
(when (require 'diminish nil 'noerror)
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode "Ab"))
  (eval-after-load "company"
    '(diminish 'company-mode "Cmp"))
  (eval-after-load "yasnippet"
    '(diminish 'yas/minor-mode "Y")))

(add-hook 'emacs-lisp-mode-hook 
  (lambda()
    (setq mode-name "el")))

(provide 'my-general)

;; my-general.el ends here
