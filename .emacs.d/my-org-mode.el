;;(require 'org-install)

(require 'org-protocol)
(require 'org-exp-blocks)

(eval-after-load "org"
  '(progn
     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; org-mode hooks
     (defun my-org-mode-hook-defun ()
       ;; make sure cua-mode is disabled
       (cua-mode -1)

       ;; flyspell unless this is my password file
       ;; (unless (string-equal (buffer-name) "secrets.org.gpg")
       ;;   (flyspell-mode 1))

       ;; Use IDO for target completion
       (setq org-completion-use-ido t)

       ;; read-only if this is my password file
       (when (string-equal (buffer-name) "secrets.org.gpg")
         (setq buffer-read-only t)))

     (add-hook 'org-mode-hook 'my-org-mode-hook-defun)

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; options

     ;; Sanitize C-a, C-e, C-k for org-mode
     (setq org-special-ctrl-a/e t)
     (setq org-special-ctrl-k t)

     ;; Never insert an empty line before a new entry
     (setq org-blank-before-new-entry nil)

     ;; Hide leading stars
     (setq org-hide-leading-stars nil)
     (set-face-foreground 'org-hide "#3f3f3f")

     ;; Information to record when a task moves to the DONE state.
     (setq org-log-done 'time)

     ;; Return activates link
     (setq org-return-follows-link t)

     ;; My todo levels
     (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)" "CANCELED(c)")
                               (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE")))

     (setq org-todo-state-tags-triggers
           (quote (("CANCELLED" ("CANCELLED" . t))
                   ("WAITING" ("WAITING" . t))
                   ("HOLD" ("WAITING" . t) ("HOLD" . t))
                   (done ("WAITING") ("HOLD"))
                   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; remember.el

     (org-remember-insinuate)

     (setq org-directory "~/org/")
     
     (setq org-default-notes-file (concat org-directory "notes.org"))

     ;; Targets include current file and any file contributing to the agenda - up to 5 levels deep
     (setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                                ("maybe.org" :maxlevel . 5)
                                (nil :maxlevel . 5)))

     (setq org-remember-templates
           `(("Todo" ?t "* TODO %?\n%U" "~/org/gtd.org" "Inkorg")
             ("Note" ?n "* %?\n%U" "~/org/gtd.org" "Inkorg")             
             ("Todo+länk" ?l "* TODO %?\n%U\n%i\n%a" "~/org/gtd.org" "Inkorg")
             ("Event" ?e ,(concat "* %^{Title}\n%i\n%a\n"
                                  "\n:PROPERTIES:\n:created: %U\n:END:")
              "~/org/gtd.org" "Möten och aktiviteter")
             ;; default for org-protocol://remember://
             (?w ,(concat "* %c\n\n%i\n\n"
                          ":PROPERTIES:\n:created: %u\n:END:" "%^{content_tags}p")
                 "~/org/notes/bookmarks.org" "Bookmarks")))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; refile

     ;; Provide refile targets as paths
     (setq org-refile-use-outline-path t)

     ;; Complete the outline path in hierarchical steps
     (setq org-outline-path-complete-in-steps nil)

     ;; Allow refile to create parent tasks with confirmation
     (setq org-refile-allow-creating-parent-nodes (quote confirm))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; agenda

     (setq org-agenda-files (list "~/org/gtd.org"))
     
     (setq org-agenda-dim-blocked-tasks t)

     (setq org-agenda-custom-commands
           (quote (("N" "Notes" tags "NOTE"
                    ((org-agenda-overriding-header "Notes")
                     (org-tags-match-list-sublevels t)))
                   (" " "Agenda"
                    ((agenda "" nil)
                     (tags "REFILE"
                           ((org-agenda-overriding-header "Tasks to Refile")
                            (org-tags-match-list-sublevels nil)))
                     (tags-todo "-CANCELLED/!"
                                ((org-agenda-overriding-header "Stuck Projects")
                                 (org-tags-match-list-sublevels 'indented)
                                 (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                     (tags-todo "-WAITING-CANCELLED/!NEXT"
                                ((org-agenda-overriding-header "Next Tasks")
                                 (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                 (org-agenda-todo-ignore-scheduled t)
                                 (org-agenda-todo-ignore-deadlines t)
                                 (org-tags-match-list-sublevels t)
                                 (org-agenda-sorting-strategy
                                  '(todo-state-down effort-up category-keep))))
                     (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                                ((org-agenda-overriding-header "Tasks")
                                 (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                                 (org-agenda-todo-ignore-scheduled t)
                                 (org-agenda-todo-ignore-deadlines t)
                                 (org-agenda-sorting-strategy
                                  '(category-keep))))
                     (tags-todo "-CANCELLED/!"
                                ((org-agenda-overriding-header "Projects")
                                 (org-agenda-skip-function 'bh/skip-non-projects)
                                 (org-agenda-todo-ignore-scheduled 'future)
                                 (org-agenda-todo-ignore-deadlines 'future)
                                 (org-agenda-sorting-strategy
                                  '(category-keep))))
                     (tags-todo "-CANCELLED/!WAITING|HOLD"
                                ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                 (org-agenda-skip-function 'bh/skip-projects-and-habits)
                                 (org-agenda-todo-ignore-scheduled t)
                                 (org-agenda-todo-ignore-deadlines t)))
                     (tags "-REFILE/"
                           ((org-agenda-overriding-header "Tasks to Archive")
                            (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
                    nil)
                   ("r" "Tasks to Refile" tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
                   ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
                    ((org-agenda-overriding-header "Stuck Projects")
                     (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                   ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
                    ((org-agenda-overriding-header "Next Tasks")
                     (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                     (org-agenda-todo-ignore-scheduled t)
                     (org-agenda-todo-ignore-deadlines t)
                     (org-tags-match-list-sublevels t)
                     (org-agenda-sorting-strategy
                      '(todo-state-down effort-up category-keep))))
                   ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                    ((org-agenda-overriding-header "Tasks")
                     (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
                   ("p" "Projects" tags-todo "-CANCELLED/!"
                    ((org-agenda-overriding-header "Projects")
                     (org-agenda-skip-function 'bh/skip-non-projects)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-todo-ignore-deadlines 'future)
                     (org-agenda-sorting-strategy
                      '(category-keep))))
                   ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
                    ((org-agenda-overriding-header "Waiting and Postponed tasks"))
                    (org-agenda-skip-function 'bh/skip-projects-and-habits)
                    (org-agenda-todo-ignore-scheduled 'future)
                    (org-agenda-todo-ignore-deadlines 'future))
                   ("A" "Tasks to Archive" tags "-REFILE/"
                    ((org-agenda-overriding-header "Tasks to Archive")
                     (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; babel

     ;; languages to load
     (setq org-babel-load-languages '((emacs-lisp . t)
                                      (haskell . t)
                                      (latex . t)))

     ;; export listings
     (setq org-export-latex-listings t)

     ;; Load necessary packages for latex
     (require 'org-latex)
     (add-to-list 'org-export-latex-packages-alist '("" "listings"))
     (add-to-list 'org-export-latex-packages-alist '("" "color"))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; iimage -- display images in your org-mode-file
     (require 'iimage)
     (add-to-list 'iimage-mode-image-regex-alist
                  (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex
                                "\\)\\]")  1))
     (defun org-toggle-iimage-in-org ()
       "display images in your org file"
       (interactive)
       (if (face-underline-p 'org-link)
           (set-face-underline-p 'org-link nil)
         (set-face-underline-p 'org-link t))
       (iimage-mode))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; Org ad hoc code, quick hacks and workarounds
     ;; ;; http://orgmode.org/worg/org-hacks.html
     
     ;; Fix a problem with saveplace.el putting you back in a folded position.
     (add-hook 'org-mode-hook
               (lambda ()
                 (when (outline-invisible-p)
                   (save-excursion
                     (outline-previous-visible-heading 1)
                     (org-show-subtree)))))))

(provide 'my-org-mode)

;; my-org-mode.el ends here
