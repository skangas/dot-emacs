;; Perl

;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  :mode (((rx (or (or ".pl" ".plx" ".cgi" ".pod")
                  (seq (* nonl) "/perl/" (* nonl)))
              eos)
          . cperl-mode))
  :interpreter (((rx (or "perl" "perl5" "miniperl")) . cperl-mode))
  :custom
  (cperl-clobber-lisp-bindings t)
  
  (cperl-lazy-help-time 1) ;; show help after x seconds
  (cperl-invalid-face nil) ;; NB. should be made obsolete

  (cperl-electric-linefeed t)
  (cperl-electric-keywords t)
  (cperl-electric-parens nil)

  ;; indentation settings
  (cperl-tab-always-indent t)            ;; TAB always indents line
  (cperl-indent-left-aligned-comments t) ;; indent left aligned comments
  (cperl-auto-newline nil)               ;; No automatic newline
  ;; (cperl-auto-newline-after-colon) ;; automatic newline after colon when above setting enabled
  (cperl-indent-level 4)
  (cperl-continued-statement-offset 4)
  (cperl-continued-brace-offset 0)
  (cperl-indent-parens-as-block t) ;; indent parenthetic expressions sanely
  (cperl-close-paren-offset -4)    ;; needed for parens-as-block
  (cperl-label-offset 0)           ;; extra indentation for line that is a label
  (cperl-brace-offset 0)           ;; braces should only get default indentation

  :config
  (require 'sk-macros)

  (defun my-cperl-customizations ()
    "cperl-mode customizations that must be done after cperl-mode loads"
    (define-key cperl-mode-map (kbd "C-M-<") 'cperl-lineup)
    (define-key cperl-mode-map (kbd "C-h f") 'cperl-perldoc)

    ;; use perl to compile
    (set (make-local-variable 'compile-command) (concat "perl -w -c " buffer-file-name))
    ;; don't ask for confirmation
    (set (make-local-variable 'compilation-read-command) nil)
    ;; turn on auto-completion of keywords
    (abbrev-mode 1)

    ;; outline-minor-mode
    ;; http://www.emacswiki.org/emacs/CPerlModeOutlineMode

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

    (defvar my-cperl-outline-regexp
      (rx bol (* (in " \t"))
          (group (or
                  (: "=head" (any "12")) ; POD header
                  "package"              ; package
                  "=item"                ; POD item
                  "sub"                  ; subroutine definition
                  "class" "has"          ; use Moose;
                  ))
          word-boundary))

    (setq cperl-outline-regexp  my-cperl-outline-regexp)
    (setq outline-regexp        cperl-outline-regexp)
    (setq outline-level        'cperl-outline-level))
  (add-hook 'cperl-mode-hook 'my-cperl-customizations t)

  ;; perltidy
  (defun perltidy-region ()
    "Run perltidy on the current region."
    (interactive)
    (save-excursion
      (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

  (defun perltidy-defun ()
    "Run perltidy on the current defun."
    (interactive)
    (save-excursion (mark-defun)
                    (perltidy-region))))

(provide 'init-lang-perl)
