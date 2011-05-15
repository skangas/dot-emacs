(setq ediff-split-window-function (lambda (&optional arg)
				    (if (> (frame-width) 150)
					(split-window-horizontally arg)
				      (split-window-vertically arg))))

(provide 'my-ediff)

;; my-ediff.el ends here
