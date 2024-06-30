;;; init-lang-scheme.el - Scheme  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'quack)
;; (quack-install)

(use-package geiser
  :defer t
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-guile)


(provide 'init-lang-scheme)

;;; init-lang-scheme.el ends here
