;;(require 'org-install)

;;(add-to-list 'load-path "/usr/share/emacs/23.1/site-lisp/org-mode/")
(require 'org-protocol)
(require 'org-exp-blocks)

  (require 'org-latex)
  (add-to-list 'org-export-latex-packages-alist '("" "listings"))
  (add-to-list 'org-export-latex-packages-alist '("" "color"))


(eval-after-load "org"
  '(progn
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;; org-mode hooks
     (defun my-org-mode-hook-defun ()
       ;; make sure cua-mode is disabled
       (cua-mode -1)

       ;; flyspell unless this is my password file
       ;; (unless (string-equal (buffer-name) "secrets.org.gpg")
       ;;   (flyspell-mode 1))


       ;; Use IDO for target completion
       (setq org-completion-use-ido t)

       ;; Use IDO only for buffers
       ;; set ido-mode to buffer and ido-everywhere to t via the customize interface
       ;; '(ido-mode (quote both) nil (ido))
       ;; '(ido-everywhere t)

       ;; read-only if this is my password file
       (when (string-equal (buffer-name) "secrets.org.gpg")
         (setq buffer-read-only t)))

     (add-hook 'org-mode-hook 'my-org-mode-hook-defun)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; remember.el
     (org-remember-insinuate)
     (setq org-directory "~/org/")
     (setq org-default-notes-file (concat org-directory "/notes.org"))

     ;; remember templates
     (setq org-remember-templates
           `(("Todo" ?t "* TODO %?
  %U
  %i
  %a" "~/org/gtd.org" "Inkorg")
             ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org")
             ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/journal.org" "Idéer")
             ("Event" ?e ,(concat "* %^{Title}\n%i\n%a\n"
                                  "\n:PROPERTIES:\n:created: %U\n:END:")
              "~/org/gtd.org" "Möten och aktiviteter")
             ;; default for org-protocol://remember://
             (?w ,(concat "* %c\n\n%i\n\n"
                          ":PROPERTIES:\n:created: %u\n:END:" "%^{content_tags}p")
                 "~/org/notes/bookmarks.org" "Bookmarks")))
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; various options 

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

     ;; http://orgmode.org/manual/Clean-view.html#Clean-view

     ;; Only use odd levels
     (setq org-odd-levels-only nil)

     ;; My todo levels
     (setq org-todo-keywords '((sequence "MAYBE(m)" "TODO(t)" "STARTED(s)" "APPT(a)" "|" "DONE(d)" "CANCELED(c)" "(f)DEFERRED")))

     ;; agenda
     (setq org-agenda-files (list "~/org/gtd.org"))

     (setq org-babel-load-languages '((emacs-lisp . t)
                                      (latex . t)))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; refiling
     
     ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
     (setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                                ("maybe.org" :maxlevel . 5)
                                (nil :maxlevel . 5)))

     ;; Targets start with the file name - allows creating level 1 tasks
     (setq org-refile-use-outline-path 'file)

     ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
     (setq org-outline-path-complete-in-steps t)

     ;; Allow refile to create parent tasks with confirmation
     (setq org-refile-allow-creating-parent-nodes (quote confirm))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; agenda
     (setq org-agenda-dim-blocked-tasks t)

     (setq org-agenda-custom-commands
           (quote (("w" "Tasks waiting on something" tags "WAITING/!"
                    ((org-use-tag-inheritance nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-with-date nil)
                     (org-agenda-overriding-header "Waiting Tasks")))
                   ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
                    ((org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-agenda-overriding-header "Tasks to Refile")))
                   ("N" "Notes" tags "NOTE"
                    ((org-agenda-overriding-header "Notes")))
                   ("n" "Next" tags-todo "-WAITING-CANCELLED/!NEXT"
                    ((org-agenda-overriding-header "Next Tasks")))
                   ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
                    ((org-agenda-skip-function 'bh/skip-non-projects)
                     (org-agenda-overriding-header "Projects")))
                   ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED-WAITING-SOMEDAY"
                    ((org-agenda-skip-function 'bh/skip-projects)
                     (org-agenda-overriding-header "Other Non-Project Tasks")))
                   ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELLED"
                    ((org-agenda-overriding-header "Tasks to  Archive")))
                   ("h" "Habits" tags-todo "STYLE=\"habit\""
                    ((org-agenda-todo-ignore-with-date nil)
                     (org-agenda-todo-ignore-scheduled nil)
                     (org-agenda-todo-ignore-deadlines nil)
                     (org-agenda-overriding-header "Habits")))
                   ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
                    ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                     (org-agenda-overriding-header "Stuck Projects")))
                   ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
                    ((org-agenda-skip-function
                      '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
                     (org-agenda-overriding-header "Set default clocking task with C-u C-u I"))))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; iimage -- display images in your org-mode-file
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

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; publish -- skangas.se. WIP
     (setq org-publish-project-alist
           '(("www"
              :author Stefan Kangas
              :email skangas@skangas.se
              :base-directory "~/org/www/"
              :publishing-directory "~/org/www/html"

              :auto-postamble nil
              :section-numbers nil
              :special-strings t
              :table-of-contents nil

              ;; ("misc"
              ;;  :base-directory "~/misc/"
              ;;  :base-extension (regexp-opt '("asc" "css"))
              ;;  :publishing-directory "~/org/www/html"
              ;;  :publishing-function org-publish-attachment)))))

              :style "<link rel=\"stylesheet\"
                     href=\"style.css\"
                     type=\"text/css\"/>")))
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;;;; Org ad hoc code, quick hacks and workarounds
     ;;;; http://orgmode.org/worg/org-hacks.html
     
     ;; Fix a problem with saveplace.el putting you back in a folded position.
     (add-hook 'org-mode-hook
               (lambda ()
                 (when (outline-invisible-p)
                   (save-excursion
                     (outline-previous-visible-heading 1)
                     (org-show-subtree)))))))

(provide 'my-org-mode)

;; my-org-mode.el ends here
