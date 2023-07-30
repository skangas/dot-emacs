;;; init-coding-common.el --- Config for programming
;;; Commentary:
;;; Code:

;;; General coding.

;; Disabled for now.
;; (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared bindings
(defun my-coding-keys (map)
  "Set key bindings for all programming languages. 
Argument MAP is the local keymap (e.g. `cperl-mode-map')."
  (define-key map (kbd "RET") #'newline-and-indent)
  (define-key map (kbd "M-?") #'indent-region))

(add-hook 'lisp-mode-hook 'my-lisp-mode-customizations t)
(defun my-lisp-mode-customizations ()
  (my-coding-keys lisp-mode-base-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation.

(use-package compile
  :custom
  (compile-command (format "make -k -j%s " (num-processors))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Codespell < 2.2.2 bug workaround.

(defun sk/compile-adjust-line-for-codespell-bug (&rest _args)
  (when (with-current-buffer compilation-last-buffer
          (and (eq major-mode 'compilation-mode)
               (save-excursion (goto-char (point-min))
                               (re-search-forward "^./codespell.sh" nil t))))
    (let ((line-offset 0))
      (save-restriction
        (narrow-to-region (point-min) (point))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (rx bol "\^L") nil t)
            (cl-incf line-offset))))
      (when (> line-offset 0)
        (forward-line (- line-offset))))))

(advice-add 'next-error :after 'sk/compile-adjust-line-for-codespell-bug)
;;(advice-add 'previous-error :after 'sk/compile-adjust-line-for-codespell-bug)
(advice-add 'compile-goto-error :after 'sk/compile-adjust-line-for-codespell-bug)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Compile command.

;; this function is used to set up compile command in both c and c++ conf
(defun my-compile-runs-makefile-or-compiler (create-compiler-command)
  "Configure compile command to run Makefile if it exists.
Otherwise use the compiler command given by passed in
CREATE-COMPILER-COMMAND."
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (when (buffer-file-name)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (funcall create-compiler-command file))))))

(add-hook 'c-mode-hook
          (lambda ()
            (my-compile-runs-makefile-or-compiler
             (lambda (file)
               (concat "gcc -O2 -Wall -o " (file-name-sans-extension file)
                       " " file)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (my-compile-runs-makefile-or-compiler
             (lambda (file)
               (concat "g++ -O2 -Wall -o " (file-name-sans-extension file)
                       " " file)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty λ-symbol.

(defun my/pretty-lambda ()
  (setq prettify-symbols-alist '(("lambda" . ?λ)))
  (prettify-symbols-mode 1))
(add-hook 'emacs-lisp-mode-hook #'my/pretty-lambda)
(add-hook 'ielm-mode-hook #'my/pretty-lambda)
(add-hook 'lisp-interaction-mode-hook #'my/pretty-lambda)
(add-hook 'scheme-mode-hook #'my/pretty-lambda)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark special words
(defun my-highlight-special-words ()
  (font-lock-add-keywords nil
                  '(("\\<\\(XXX\\|FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'c-mode-common-hook 'my-highlight-special-words)
(set-face-underline 'font-lock-warning-face "yellow")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Packages.

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package debbugs
  :pin "gnu"
  :commands (debbugs-gnu)
  :custom
  (debbugs-gnu-branch-directory "~/wip/emacs-release")
  (debbugs-gnu-trunk-directory "~/wip/emacs")
  :config
  ;; workaround to send control messages...
  (autoload 'sendmail-send-it "sendmail" nil t))

(use-package diff-mode                  ; (built-in)
  :bind (("C-j" . 'diff-goto-source)))

;; (use-package diff-hl
;;   :config
;;   (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;   (add-hook 'prog-mode-hook 'diff-hl-flydiff-mode)
;;   (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package elide-head
  :ensure nil                           ; built-in
  :preface
  (when (< emacs-major-version 29)
    (defalias 'elide-head 'elide-head-mode))
  :hook (prog-mode . elide-head-mode))

(use-package el-search
  :defer t)

(use-package flymake
  :hook ((emacs-lisp-mode . flymake-mode)
         (texinfo-mode . flymake-mode))
  :bind ( :map flymake-mode
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error)))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :commands haskell-mode
  :config
  (require 'haskell-interactive-mode)
  (defun my-haskell-newline-and-indent ()
    "The default behavior of `newline-and-indent in haskell-mode is annoying.
This will run newline-and-indent, and then indent once more."
    (interactive)
    (newline-and-indent)
    (indent-for-tab-command)
    (indent-for-tab-command))
  (defun my-haskell-customizations ()
    "haskell-mode customizations that must be done after haskell-mode loads"
    (defvar multiline-flymake-mode nil)
    (defvar flymake-split-output-multiline nil)
    (define-key haskell-mode-map (kbd "RET") 'my-haskell-newline-and-indent)
    (flymake-mode))
  (add-hook 'haskell-mode-hook 'my-haskell-customizations t)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)


  (defun flymake-Haskell-init ()
    (flymake-simple-make-init-impl
     'flymake-create-temp-with-folder-structure nil nil
     (file-name-nondirectory buffer-file-name)
     'flymake-get-Haskell-cmdline))

  (defun flymake-get-Haskell-cmdline (source base-dir)
    (list "ghc"
          (list "--make" "-fbyte-code"
                (concat "-i"base-dir)  ;;; can be expanded for additional -i options as in the Perl script
                source)))

  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-refac-mode)
  ;; (add-hook 'haskell-mode-hook 'hs-lint-mode-hook)

  ;; show error below
  (when (fboundp 'resize-minibuffer-mode) ; for old emacs
    (resize-minibuffer-mode)
    (setq resize-minibuffer-window-exactly nil))


  ;; TODO what does this do?
  (defvar multiline-flymake-mode nil)
  (defvar flymake-split-output-multiline nil)

  ;; this needs to be advised as flymake-split-string is used in other places and
  ;; I don't know of a better way to get at the caller's details
  (defadvice flymake-split-output
      (around flymake-split-output-multiline activate protect)
    (if multiline-flymake-mode
        (let ((flymake-split-output-multiline t))
          ad-do-it)
      ad-do-it))

  (defadvice flymake-split-string
      (before flymake-split-string-multiline activate)
    (when flymake-split-output-multiline
      (ad-set-arg 1 "^\\s *$")))

  (add-hook
   'haskell-mode-hook
   (lambda ()
        ;;; use add-to-list rather than push to avoid growing the list for every Haskell file loaded
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
     (add-to-list 'flymake-err-line-patterns
                  '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
                    1 2 3 4))
     (set (make-local-variable 'multiline-flymake-mode) t)))

  (defun credmp/flymake-display-err-minibuf ()
    "Displays the error/warning for the current line in the minibuffer"
    (interactive)
    (let* ((line-no             (flymake-current-line-no))
           (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count               (length line-err-info-list))
           )
      (while (> count 0)
        (when line-err-info-list
          (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                 (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
            (message "[%s] %s" line text)
            )
          )
        (setq count (1- count)))))

  ;; bind the above function to C-c d
  (add-hook
   'haskell-mode-hook
   (lambda ()
     (define-key haskell-mode-map "\C-cd"
                 'credmp/flymake-display-err-minibuf))))

(use-package macrostep
  :defer t)

(use-package magit
  :bind (("C-c g" . magit-dispatch))
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-repository-directories '(("~/wip/emacs/" . 0))))

(use-package paredit
  :hook (( emacs-lisp-mode ielm-mode
           lisp-data-mode lisp-mode lisp-interaction-mode
           scheme-mode)
         . enable-paredit-mode)
  :diminish "PEd"
  :config
  ;; make eldoc aware of paredit
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))

(use-package php-mode
  :mode ("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
  :config
  (defun my-php-mode-customizations ()
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (concat "php -l " file)))
    (setq c-basic-offset 4)
    (setq tab-width 4)
    (setq indent-tabs-mode nil)
    ;; Don't expand tabs when deleting them
    (setq backward-delete-function nil))
  (add-hook 'php-mode-hook 'my-php-mode-customizations))

(use-package projectile
  :disabled t
  :defer 5
  :pin "melpa"
  :diminish
  :bind ( :map projectile-mode
          ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching t)
  (projectile-globally-ignored-file-suffixes '(".elc")))

(use-package smerge-mode
  :defer t
  :custom
  (smerge-command-prefix (kbd "C-c v"))

  :after hydra

  ;; This hydra taken from
  ;; https://github.com/alphapapa/unpackaged.el#hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)
                                     (recenter-top-bottom)))))

(use-package web-mode
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.vue\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :interpreter ("node" . web-mode))

(use-package ws-butler
  :hook prog-mode-hook
  :custom
  (ws-butler-convert-leading-tabs-or-spaces t))

(use-package xr
  :defer t)

(use-package yaml-mode
  :mode (("\\.ya?ml\\'" . yaml-mode)))

(provide 'init-coding-common)

;;; init-coding-common.el ends here
