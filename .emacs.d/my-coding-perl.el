;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERL

;; (defun flymake-perl-init (buffer)
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                      buffer 'flymake-create-temp-inplace))
;;          (local-file  (concat (flymake-build-relative-filename
;;                                (file-name-directory
;;                                 (buffer-file-name
;;                                  (current-buffer)))
;;                                (file-name-directory temp-file))
;;                               (file-name-nondirectory temp-file))))
;;     (list "perl" (list "-wc " local-file))))

;; ;; (setq flymake-allowed-file-name-masks
;; ;;       (cons '(".+\\.pl$"
;; ;;               flymake-perl-init
;; ;;               flymake-simple-cleanup
;; ;;               flymake-get-real-file-name)
;; ;;             flymake-allowed-file-name-masks))
;; (set (make-local-variable flymake-err-line-patterns)
;;      (cons '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]"
;;              2 3 nil 1)
;;            flymake-err-line-patterns))

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go to the correct file when using ffap on a perl module
(eval-after-load "ffap" '(require 'ffap-perl-module))

;; template-toolkit-mode
(autoload 'tt-mode "tt-mode" "TT Mode." t)
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

;; yaml-mode
(autoload 'yaml-mode "yaml-mode" "YAML Mode." t)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; http://www.emacswiki.org/emacs/CPerlModeOutlineMode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sk-macros)
(eval-after-load "cperl-mode"
  '(progn
     (setq cperl-clobber-lisp-bindings t)
     (setq cperl-highlight-variables-indiscriminately t) ;; hilight all scalars
     (setq cperl-lazy-help-time 1) ;; show help after x seconds
     (setq cperl-invalid-face nil) ;; don't show trailing whitespace with _
     (setq cperl-highlight-variables-indiscriminately nil)

     (setq cperl-electric-linefeed t)
     (setq cperl-electric-keywords t)
     (setq cperl-electric-parens nil)

     ;; indentation settings
     (setq cperl-tab-always-indent t)            ;; TAB always indents line
     (setq cperl-indent-left-aligned-comments t) ;; indent left aligned comments
     (setq cperl-auto-newline nil)               ;; No automatic newline
     (setq cperl-auto-newline-after-colon)       ;; automatic newline after colon when above setting enabled
     (setq cperl-indent-level 4)
     (setq cperl-continued-statement-offset 4)
     (setq cperl-continued-brace-offset 0)
     (setq cperl-indent-parens-as-block t)       ;; indent parenthetic expressions sanely
     (setq cperl-close-paren-offset -4)          ;; needed for parens-as-block
     (setq cperl-label-offset 0)                 ;; extra indentation for line that is a label
     (setq cperl-brace-offset 0)                 ;; braces should only get default indentation

     (add-hook 'cperl-mode-hook 'my-cperl-customizations t)))

(defun my-cperl-customizations ()
  "cperl-mode customizations that must be done after cperl-mode loads"
  (my-coding-keys cperl-mode-map)
  (define-key cperl-mode-map (kbd "C-M-<") 'cperl-lineup) ;; TODO: translate to other languages
  (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc) ;; TODO: translate to other languages

  ;; flyspell in comments
  (flyspell-prog-mode)

  ;; use perl to compile
  (set (make-local-variable 'compile-command) (concat "perl -w -c " buffer-file-name))
  ;; don't ask for confirmation
  (set (make-local-variable 'compilation-read-command) nil)
  ;; turn on auto-completion of keywords
  (abbrev-mode 1)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; flymake

  ;; outline-minor-mode
  (outline-minor-mode)

  (defun cperl-outline-level ()
    (looking-at outline-regexp)
    (let ((match (match-string 1)))
      (cond
       ((eq match "=head1" ) 1)
       ((eq match "package") 2)
       ((eq match "class"  ) 3)
       ((eq match "=head2" ) 4)
       ((eq match "=item"  ) 5)
       ((eq match "sub"    ) 6)
       ((eq match "has"    ) 7)
       (t 8)
       )))

  (setq my-cperl-outline-regexp
        (concat
         "^"                            ; Start of line
         "[ \\t]*"                      ; Skip leading whitespace
         "\\("                          ; begin capture group \1
         (sk-join "\\|"
                  '("=head[12]"              ; POD header
                    "package"                ; package
                    "=item"                  ; POD item
                    "sub"                    ; subroutine definition
                    "class" "has"            ; use Moose;
                    ))
         "\\)"                          ; end capture group \1
         "\\b"                          ; Word boundary
         ))
  ;; (flymake-mode)
  (setq cperl-outline-regexp  my-cperl-outline-regexp)
  (setq outline-regexp        cperl-outline-regexp)
  (setq outline-level        'cperl-outline-level))

;;; perltidy
(defun perltidy-region ()
  "Run perltidy on the current region."
  (interactive)
  (save-excursion
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)))
(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
		  (perltidy-region)))

(provide 'my-coding-perl)

;; my-coding-perl.el ends here
