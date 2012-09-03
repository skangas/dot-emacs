;;(require 'org-install)

(require 'org-protocol)
(require 'org-exp-blocks)

(eval-after-load "org"
  '(progn
     (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; MobileOrg
     (setq org-mobile-directory "~/Dropbox/mobileorg")
     ;(setq org-mobile-files "~/org/todo.org")

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; org-mode hooks
     (defun my-org-mode-hook-defun ()
       ;; make sure cua-mode is disabled
       (cua-mode -1)

       ;; Undefine C-c [ and C-c ] since this breaks my
       ;; org-agenda files. when directories are included it
       ;; expands the files in the directories individually.
       (org-defkey org-mode-map "\C-c["    'undefined)
       (org-defkey org-mode-map "\C-c]"    'undefined)

       ;; flyspell unless this is my password file
       ;; (unless (string-equal (buffer-name) "secrets.org.gpg")
       ;;   (flyspell-mode 1))

       ;; Use IDO for target completion
       (setq org-completion-use-ido t)

       ;; read-only if this is my password file
       (when (string-equal (buffer-name) "secrets.org.gpg")
         (setq buffer-read-only t)))

     (add-hook 'org-mode-hook 'my-org-mode-hook-defun)

     ;; Save all org-mode buffers every hour
     (run-at-time "00:59" 3600 'org-save-all-org-buffers)

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; options

     ;; Disable priority commands
     (setq org-enable-priority-commands nil)

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

     ;; Tags with fast selection keys
     (setq org-tag-alist (quote ((:startgroup)
                                 ("@ärende" . ?ä)
                                 ("@skola" . ?s)
                                 ("@hemma" . ?h)
                                 ("@lokalen" . ?l)
                                 (:endgroup)
                                 ("SKOLA" . ?S)
                                 ("AVANTI" . ?A)
                                 ("FRIPOST" . ?F)
                                 ("PERSONAL" . ?P)

                                 ("REFILE" . ?r)
                                 ("WAITING" . ?w)
                                 ("HOLD" . ?h)
                                 ;; ("WORK" . ?W)
                                 ("NOTE" . ?n)
                                 ("CANCELLED" . ?c)
                                 ("FLAGGED" . ??))))

     (setq org-todo-state-tags-triggers
           (quote (("CANCELLED" ("CANCELLED" . t))
                   ("WAITING" ("WAITING" . t))
                   ("HOLD" ("WAITING" . t) ("HOLD" . t))
                   (done ("WAITING") ("HOLD"))
                   ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                   ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

     ;; disable the default org-mode stuck projects agenda view
     (setq org-stuck-projects (quote ("" nil nil "")))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; reftex
     ;; (defun org-mode-reftex-setup ()
     ;;   (load-library "reftex")
     ;;   (and (buffer-file-name) (file-exists-p (buffer-file-name))
     ;;        (progn
     ;;          (reftex-parse-all))))
     ;; (add-hook 'org-mode-hook 'org-mode-reftex-setup)

     ;; ;;;; use latexmk to generate pdf (needed for bibtex to work)
     ;; ;; sudo apt-get install latexmk
     ;; (setq org-latex-to-pdf-process (list "latexmk -f -pdf %f"))

     ;; (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
     ;; (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)

     ;; (setq reftex-default-bibliography
     ;;       '("default.bib" "referenser.bib"))

     ;; (setq org-latex-to-pdf-process
     ;;       '("pdflatex -interaction nonstopmode -output-directory %o %f"
     ;;         "bibtex %f"
     ;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
     ;;         "pdflatex -interaction nonstopmode -output-directory %o %f"))

     ;; http://www-public.it-sudparis.eu/~berger_o/weblog/2012/03/23/how-to-manage-and-export-bibliographic-notesrefs-in-org-mode/
     ;; (defun my-rtcite-export-handler (path desc format)
     ;;   (message "my-rtcite-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
     ;;   (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
     ;;                    (match-string 1 path)))
     ;;          (path (substring path 0 (match-beginning 0))))
     ;;     (cond ((eq format 'latex)
     ;;            (if (or (not desc)
     ;;                    (equal 0 (search "rtcite:" desc)))
     ;;                (format "\\cite{%s}" search)
     ;;              (format "\\cite[%s]{%s}" desc search))))))

     ;; (org-add-link-type "rtcite"
     ;;                    'org-bibtex-open
     ;;                    'my-rtcite-export-handler)

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; remember.el

     (org-remember-insinuate)

     (setq org-directory "~/org/")
     
     (setq org-default-notes-file (concat org-directory "notes.org"))

     (setq org-remember-templates
           `(("Todo" ?t "* NEXT %?\n%U" "~/org/refile.org")
             ("Länk" ?l "* NEXT %?\n%U\n%a\n  %i" "~/org/refile.org")
             ("Note" ?n "* %? :NOTE:\n%U" "~/org/refile.org")             
             ;; default for org-protocol://remember://
             (?w ,(concat "* %c\n%U\n%i"
                          "%^{content_tags}p")
                 "~/org/notes/bookmarks.org" "Bookmarks")))

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; refile

     ;; Provide refile targets as paths
     (setq org-refile-use-outline-path t)

     ;; Complete the outline path in hierarchical steps
     (setq org-outline-path-complete-in-steps nil)

     ;; Allow refile to create parent tasks with confirmation
     (setq org-refile-allow-creating-parent-nodes (quote confirm))

     ;; Targets include current file and any file contributing to the agenda - up to 5 levels deep
     (setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                                (nil :maxlevel . 5)))

     ;; Exclude DONE state tasks from refile targets
     (defun bh/verify-refile-target ()
       "Exclude todo keywords with a done state from refile targets"
       (not (member (nth 2 (org-heading-components)) org-done-keywords)))

     (setq org-refile-target-verify-function 'bh/verify-refile-target)

     ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; ;; agenda

     (setq org-agenda-files '("~/org/"))
     
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
                                 ;; (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
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
                     ;; (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
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


     ;; Load necessary packages for latex
     (require 'org-latex)

     ;; export listings
     (setq org-export-latex-listings t)

     ;; export listings
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

;; Helper functions from section 6.1 of Organize Your Life In Plain Text!

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;; Helper functions from section 11.2 of Organize Your Life In Plain Text!

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next (save-excursion
                             (forward-line 1)
                             (and (< (point) subtree-end)
                                  (re-search-forward "^\\*+ \\(NEXT\\) " subtree-end t)))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (if (bh/is-project-p)
              nil
            subtree-end)))
    (org-end-of-subtree t)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

;;;;; FIXME: This should be removed with an upgraded org-mode
(when (string= org-version "TAG=7.01g") 
 (defun org-is-habit-p (&optional pom)
   "Is the task at POM or point a habit?"
   (string= "habit" (org-entry-get (or pom (point)) "STYLE"))))

(provide 'my-org-mode)

;; my-org-mode.el ends here
