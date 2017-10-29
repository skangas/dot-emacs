;;(require 'org-install)

(require 'org-protocol)
;; TODO: 18.4 Checklist handling
;;       (require 'org-checklist)
;; (require 'org-exp-blocks)

(eval-after-load "org"
  '(progn
     (add-to-list 'org-modules 'org-habit)
     (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

     ;; MobileOrg
     (setq org-mobile-directory "~/Dropbox/mobileorg")
     ;(setq org-mobile-files "~/org/todo.org")

     ;; org-mode hooks
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

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; options

     ;; Disable priority commands
     (setq org-enable-priority-commands nil)

     ;; Sanitize C-a, C-e, C-k for org-mode
     (setq org-special-ctrl-a/e t)
     (setq org-special-ctrl-k t)

     ;; Never insert an empty line before a new entry
     (setq org-blank-before-new-entry nil)

     ;; Hide leading stars
     (setq org-hide-leading-stars t)
     (set-face-foreground 'org-hide "#3f3f3f")

     ;; hide italics markers
     (setq org-hide-emphasis-markers t)

     ;; Information to record when a task moves to the DONE state.
     (setq org-log-done 'time)

     ;; Return activates link
     (setq org-return-follows-link t)

     ;; Strike out done headlines
     (setq org-fontify-done-headline t)
     (custom-set-faces
      '(org-headline-done ((t (:strike-through "#222")))))

     ;; General font customization
     (custom-set-faces
      '(org-document-title ((t (:height 1.4 :family "Lucida Grande" :weight bold)))))

     ;; My todo levels
     (setq org-todo-keywords
           '((sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "|" "DONE(d!/!)" "CANCELLED(c)")
             (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

     ;; Tags with fast selection keys
     (setq org-tag-alist (quote ((:startgroup)
                                 ("@centret" . ?c)
                                 ("@hemma" . ?h)
                                 ("@telefon" . ?t)
                                 ("@ärende" . ?ä)
                                 (:endgroup)
                                 ("EK" . ?E)
                                 ("PERSONAL" . ?P)

                                 ("REFILE" . ?r)
                                 ("WAITING" . ?w)
                                 ("HOLD" . ?h)
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

     ;; keep the agenda fast
     (setq org-agenda-span 'day)

     ;; Use sticky agenda's so they persist
     (setq org-agenda-sticky t)

     ;; disable the default org-mode stuck projects agenda view
     (setq org-stuck-projects (quote ("" nil nil "")))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; org-capture

     (load "~/org/.org-capture.el")

     (setq org-directory "~/org/")
     (setq org-default-notes-file (concat org-directory "notes.org"))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; refile

     ;; Provide refile targets as paths
     (setq org-refile-use-outline-path t)

     ;; Complete the outline path in hierarchical steps
     (setq org-outline-path-complete-in-steps nil)

     ;; Allow refile to create parent tasks with confirmation
     (setq org-refile-allow-creating-parent-nodes (quote confirm))

     ;; Targets include current file and any file contributing to the agenda - up to 5 levels deep
     (setq org-refile-targets '((org-agenda-files :maxlevel . 5)
                                ("~/org/later.org" :maxlevel . 5)))

     ;; Exclude DONE state tasks from refile targets
     (defun bh/verify-refile-target ()
       "Exclude todo keywords with a done state from refile targets"
       (not (member (nth 2 (org-heading-components)) org-done-keywords)))
     (setq org-refile-target-verify-function 'bh/verify-refile-target)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; agenda

     (setq org-agenda-files '("~/org/todo.org"
                              "~/org/personal.org"
                              "~/org/refile.org"))
     
     (setq org-agenda-dim-blocked-tasks t)
     (setq org-agenda-tags-todo-honor-ignore-options t)

     (setq org-agenda-custom-commands
           (quote (("N" "Notes" tags "NOTE"
                    ((org-agenda-overriding-header "Notes")
                     (org-tags-match-list-sublevels t)))
                   ("h" "Habits" tags-todo "STYLE=\"habit\""
                    ((org-agenda-overriding-header "Habits")
                     (org-agenda-sorting-strategy
                      '(todo-state-down effort-up category-keep))))
                   (" " "Agenda"
                    ((agenda "" nil)
                     (tags "REFILE"
                           ((org-agenda-overriding-header "Tasks to Refile")
                            (org-tags-match-list-sublevels nil)))
                     (tags-todo "-CANCELLED/!"
                                ((org-agenda-overriding-header "Stuck Projects")
                                 (org-agenda-skip-function 'bh/skip-non-stuck-projects))
                                (org-agenda-sorting-strategy
                                 '(category-keep)))
                     (tags-todo "-HOLD-CANCELLED/!"
                                ((org-agenda-overriding-header "Projects")
                                 (org-agenda-skip-function 'bh/skip-non-projects)
                                 (org-tags-match-list-sublevels 'indented)
                                 (org-agenda-sorting-strategy
                                  '(category-keep))))
                     (tags-todo "-CANCELLED/!NEXT"
                                ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                       (if bh/hide-scheduled-and-waiting-next-tasks
                                                                           ""
                                                                         " (including WAITING and SCHEDULED tasks)")))
                                 (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                 (org-tags-match-list-sublevels t)
                                 (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-sorting-strategy
                                  '(todo-state-down effort-up category-keep))))
                     (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                       (if bh/hide-scheduled-and-waiting-next-tasks
                                                                           ""
                                                                         " (including WAITING and SCHEDULED tasks)")))
                                 (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                 (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-sorting-strategy
                                  '(category-keep))))
                     (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                       (if bh/hide-scheduled-and-waiting-next-tasks
                                                                           ""
                                                                         " (including WAITING and SCHEDULED tasks)")))
                                 (org-agenda-skip-function 'bh/skip-project-tasks)
                                 (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-sorting-strategy
                                  '(category-keep))))
                     (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                       (if bh/hide-scheduled-and-waiting-next-tasks
                                                                           ""
                                                                         " (including WAITING and SCHEDULED tasks)")))
                                 (org-agenda-skip-function 'bh/skip-non-tasks)
                                 (org-tags-match-list-sublevels nil)
                                 (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                 (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                     (tags "-REFILE/"
                           ((org-agenda-overriding-header "Tasks to Archive")
                            (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                            (org-tags-match-list-sublevels nil))))
                    nil))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; babel

     ;; languages to load

     (setq org-babel-load-languages '((emacs-lisp . t)
                                      (haskell . t)
                                      (latex . t)))


     ;; ;; Load necessary packages for latex
     ;; (require 'org-latex)

     ;; ;; export listings
     ;; (setq org-export-latex-listings t)

     ;; ;; export listings
     ;; (add-to-list 'org-export-latex-packages-alist '("" "listings"))
     ;; (add-to-list 'org-export-latex-packages-alist '("" "color"))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; iimage -- display images in your org-mode-file

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

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Org ad hoc code, quick hacks and workarounds
     ;; http://orgmode.org/worg/org-hacks.html
     
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

;; Helper functions from section 14.2 of Organize Your Life In Plain Text!

(setq org-stuck-projects (quote ("" nil nil "")))

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

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

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

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

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
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
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

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
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
