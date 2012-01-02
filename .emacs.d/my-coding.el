;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General coding

(autoload 'magit-status "magit" "magit" t nil)

;; make all scripts executable when saving
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Shared bindings
(defun my-coding-keys (map)
  "Sets the key bindings which shall be available in all programming
  languages. Argument MAP is the local keymap (e.g. cperl-mode-map)."
  (define-key map (kbd "RET")                 'newline-and-indent)
  (define-key map (kbd "M-?")                 'indent-region))

;; Mark special words
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords nil
              '(("\\<\\(XXX\\|FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

;(set-face-underline 'font-lock-warning-face "yellow")

(setq compile-command "make -k -j5 ")

(setq glasses-separate-parentheses-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(defun my-enable-paredit-mode ()
  (paredit-mode +1))
(add-hook 'emacs-lisp-mode-hook       'my-enable-paredit-mode)
(add-hook 'lisp-mode-hook             'my-enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'my-enable-paredit-mode)
(add-hook 'scheme-mode-hook           'my-enable-paredit-mode)

;; make eldoc aware of paredit
(eval-after-load 'eldoc
  '(progn (eldoc-add-command
           'paredit-backward-delete
           'paredit-close-round)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gud

;Add color to the current GUD line
(defvar my-gud-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")
(defadvice gud-display-line (after my-gud-highlight act)
  "Highlight current line."
  (let* ((ov my-gud-overlay)
         (bf (gud-find-file (ad-get-arg 0))))
    (save-excursion
      (set-buffer bf)
      (move-overlay ov (line-beginning-position) (line-beginning-position 2)
                    (current-buffer)))))
(defun my-gud-kill-buffer ()
  (if (eq major-mode 'gud-mode)
      (delete-overlay my-gud-overlay)))
(add-hook 'kill-buffer-hook 'my-gud-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ecb

;; (require 'ecb-autoloads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake

(autoload 'flymake-mode "flymake" "flymake mode" t nil)
(eval-after-load "flymake"
  '(progn
     (defun my-flymake-show-next-error()
       (interactive)
       (flymake-goto-next-error)
       (flymake-display-err-menu-for-current-line))
     
     (add-hook 'flymake-mode-hook
               (local-set-key (kbd "C-c C-e") 'my-flymake-show-next-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar

;; (require 'sr-speedbar)

;; TODO: go to correct window
;; (defun my-close-speedbar-fun ()
;;   (interactive)
;;   (when (string-equal major-mode "speedbar-mode")
;;     ;; (let ((sb-frame (speedbar-current-frame)))
;;     ;;   (dframe-select-attached-frame sb-frame)
;;       (delete-window)))
;;       ;; (select-frame back-frame))))

;; (setq speedbar-mode-hook
;;       (lambda ()
;;         (define-key speedbar-key-map (kbd "q") 'my-close-speedbar-fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile command

;; this function is used to set up compile command in both c and c++ conf

(defun my-compile-runs-makefile-or-compiler (create-compiler-command)
  "Configure compile command to run Makefile if it exists, or
otherwise use the compiler command given by passed in
compiler-command."
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (when (buffer-file-name)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (funcall create-compiler-command file))))))

(provide 'my-coding)

;; my-coding.el ends here
