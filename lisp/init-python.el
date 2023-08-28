;;; init-python.el --- Python -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package pip-requirements
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; venv

(use-package pyvenv
  :config
  (pyvenv-mode))

;; (use-package auto-virtualenv
;;   :hook
;;   ((python-mode . auto-virtualenv-set-virtualenv)
;;    (window-configuration-change . auto-virtualenv-set-virtualenv)
;;    (after-focus-change . auto-virtualenv-set-virtualenv)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; black

;; https://github.com/wbolster/emacs-python-black
;; https://github.com/pythonic-emacs/blacken

(use-package blacken
  :hook python-mode
  :custom
  (blacken-target-version "py310"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-docstring

(use-package python-docstring
  :hook python-mode)

(use-package flymake-ruff
  :init
  (defun sk/flymake-ruff-load-for-python ()
    (if (derived-mode-p 'python-mode)
        (flymake-ruff-load)))
  :hook (eglot-managed-mode . sk/flymake-ruff-load-for-python))

(use-package eglot
  :ensure nil
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pyright LSP

;; Courtesy of https://robbmann.io/posts/emacs-eglot-pyrightconfig/
(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")

  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/.venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above .venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `.venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json' is in the project root
         ;; folder.
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

         ;; Finally, get a string with the JSON payload.
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
    (with-temp-file out-file (insert out-contents "\n"))))


(provide 'init-python)

;;; init-python.el ends here
