;;; init-org-roam.el                                                 -*- lexical-binding: t; -*-

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n l" . org-roam-buffer-toggle))
  :custom
  (org-roam-directory (expand-file-name "~/org/roam/"))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode))

;; (global-set-key (kbd "C-c n") nil)

(provide 'init-org-roam)

