;;; init-org-mode.el                                                 -*- lexical-binding: t; -*-

(use-package restclient
  :defer t)


(use-package ob-restclient
  :defer t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (restclient . t)))

(use-package org
  :bind (("C-c å" . sk/toggle-next-task-display))
  :hook
  (org-mode . visual-line-mode)
  :custom
  (org-footnote-auto-adjust t)
  (org-return-follows-link t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)

  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "MAYBE(m)" "|" "DONE(d!/!)" "CANCELLED(c!)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "ONGOING(o)" "|" "CANCELLED(c!)")))

  (org-tag-alist (quote ((:startgroup)
                         ("@centret" . ?c)
                         ("@hemma" . ?h)
                         ("@telefon" . ?t)
                         ("@ärende" . ?ä)
                         ("@ek" . ?e)
                         ("@ck" . ?k)
                         ("@grupp" . ?g)
                         ("@moten" . ?m)
                         ("@lasning" . ?l)
                         ("@laptop" . ?p)
                         ("@spanska" . ?s)
                         ("emacs" . ?x)
                         ("@fritid" . ?f)
                         (:endgroup)
                         ("PERSONAL" . ?P)
                         ("REFILE" . ?r)
                         ("WAITING" . ?w)
                         ("HOLD" . ?h)
                         ("NOTE" . ?n)
                         ("CANCELLED" . ?c)
                         ("FLAGGED" . ??))))

  (org-agenda-files '("~/org/refile.org"
                      "~/org/agenda.org"
                      "~/org/todo.org"
                      "~/org/personal.org"
                      ))
  ;;(org-agenda-skip-deadline-if-done t)
  ;;(org-agenda-skip-scheduled-if-done t)
  (org-clock-out-remove-zero-time-clocks t)

  (org-html-validation-link nil)

  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archived Tasks")
  :config

  (require 'org-protocol)

  ;; Type ‘<s’ to get ‘#+BEGIN_SRC’ ... ‘#+END_SRC’
  (when (not (version< org-version "9.2"))
    (require 'org-tempo))

  ;; Link to notmuch.
  (ignore-errors (require 'ol-notmuch))  ; 9.3.1 and later
  (ignore-errors (require 'org-notmuch)) ; 9.3 and earlier (27.1)

  ;; Link to man pages.
  (ignore-errors (require 'ol-man))     ; 9.3.1 and later
  (ignore-errors (require 'org-man))    ; 9.3 and earlier (27.1)

  ;; My GMail link type.
  (org-add-link-type
   "gmail"
   (lambda (link)
     (browse-url
      (concat "https://mail.google.com/mail/?shva=1#all/" link))))


  )

(use-package org-super-agenda
  :ensure t
  :custom
  (org-agenda-custom-commands
   '(("w" "Work"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :date today
                             :todo "TODAY"
                             :scheduled today
                             :order 1)))))
       (alltodo "" ((org-agenda-overriding-header "")
                    ;; (tags "-@emacs")
                    (org-super-agenda-groups
                     '((:name "Emacs"
                              :tag "@emacs"
                              :order 13)
                       (:name "Next to do"
                              :todo "NEXT"
                              :order 1)
                       (:name "Important"
                              :tag "Important"
                              :priority "A"
                              :order 6)
                       (:name "Due Today"
                              :deadline today
                              :order 2)
                       (:name "Due Soon"
                              :deadline future
                              :order 8)
                       (:name "Overdue"
                              :deadline past
                              :order 7)
                       (:name "Assignments"
                              :tag "Assignment"
                              :order 10)
                       (:name "Issues"
                              :tag "Issue"
                              :order 12)
                       (:name "Projects"
                              :tag "Project"
                              :order 14)

                       (:name "Research"
                              :tag "Research"
                              :order 15)
                       (:name "To read"
                              :tag "Read"
                              :order 30)
                       (:name "Waiting"
                              :todo "WAITING"
                              :order 20)
                       (:name "trivial"
                              :priority<= "C"
                              :tag ("Trivial" "Unimportant")
                              :todo ("SOMEDAY" )
                              :order 90)
                       (:discard (:tag ("Chore" "Routine" "Daily")))))))))
     ("z" "Super zaen view"
      ((agenda "" ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Today"
                             :time-grid t
                             :date today
                             :todo "TODAY"
                             :scheduled today
                             :order 1)))))
       (alltodo "" ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "Next to do"
                              :todo "NEXT"
                              :order 1)
                       (:name "Important"
                              :tag "Important"
                              :priority "A"
                              :order 6)
                       (:name "Due Today"
                              :deadline today
                              :order 2)
                       (:name "Due Soon"
                              :deadline future
                              :order 8)
                       (:name "Overdue"
                              :deadline past
                              :order 7)
                       (:name "Assignments"
                              :tag "Assignment"
                              :order 10)
                       (:name "Issues"
                              :tag "Issue"
                              :order 12)
                       (:name "Projects"
                              :tag "Project"
                              :order 14)
                       (:name "Emacs"
                              :tag "Emacs"
                              :order 13)
                       (:name "Research"
                              :tag "Research"
                              :order 15)
                       (:name "To read"
                              :tag "Read"
                              :order 30)
                       (:name "Waiting"
                              :todo "WAITING"
                              :order 20)
                       (:name "trivial"
                              :priority<= "C"
                              :tag ("Trivial" "Unimportant")
                              :todo ("SOMEDAY" )
                              :order 90)
                       (:discard (:tag ("Chore" "Routine" "Daily")))))))))
     ("e" "Emacs"
      ((agenda "-CANCELLED"
               ((org-agenda-span 'week)))
       (alltodo "emacs"
                ((org-agenda-overriding-header "Emacs")
                 (org-agenda-todo-ignore-scheduled 'all)
                 (org-agenda-todo-ignore-deadlines 'all)
                 ;; (org-agenda-todo-ignore-with-date t)
                 (org-super-agenda-groups
                  '((:name "Prioriterade uppgifter"
                           :priority "A")
                    (:name "Att göra"
                           :todo "TODO")
                    (:name "Kanske"
                           :todo "MAYBE"))))))
      ((org-agenda-tag-filter-preset '("+emacs"))))))
  :config
  (org-super-agenda-mode 1))


;;; Hide scheduled and waiting next tasks

(defvar sk/hide-scheduled-and-waiting-next-tasks t)

(defun sk/toggle-next-task-display ()
  (interactive)
  (setq sk/hide-scheduled-and-waiting-next-tasks (not sk/hide-scheduled-and-waiting-next-tasks))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if sk/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(provide 'init-org-mode)
