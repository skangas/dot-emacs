;; C

(defun my-c-mode-hook-customizations ()
  (c-set-offset 'arglist-intro '+))

(defun my/c-mode-customizations ()
  (interactive)
  (setq c-file-style "linux"
        c-basic-offset 8
        tab-width 8
        indent-tabs-mode t
        ;; ws-butler-convert-leading-tabs-or-spaces nil
        )
  (setq paragraph-start "[      ]*\\(//+\\|\\**\\)[     ]*$\\|^\f")
  ;; brackets should be at same indentation level as the statements they open
  (c-set-offset 'substatement-open '0))

(add-hook 'c-mode-hook #'my/c-mode-customizations)

(provide 'init-lang-c)
