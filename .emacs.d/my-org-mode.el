;;(require 'org-install)

(add-to-list 'load-path "/usr/share/emacs/23.1/site-lisp/org-mode/")
(require 'org-protocol)

(eval-after-load "org"
  '(progn

     ;; set up remember
     (org-remember-insinuate)
     (setq org-directory "~/org/")
     (setq org-default-notes-file (concat org-directory "/notes.org"))

     ;; remember templates
     (setq org-remember-templates
           `(("Todo" ?t ,(concat "* TODO %?\n  %i\n  %a"
                                 "\n  :PROPERTIES:\n  :created: %U\n  :END:")
              "~/org/todo.org" "Inkommande")
             ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/org/journal.org")
             ("Idea" ?i "* %^{Title}\n  %i\n  %a" "~/org/journal.org" "Idéer")
             ("Event" ?e ,(concat "* %^{Title}\n  %i\n  %a\n"
                                  "\n  :PROPERTIES:\n  :created: %U\n  :END:")
              "~/org/agenda.org" "Händelser")
             ;; default for org-protocol://remember://
             (?w ,(concat "* %:description\n\n  Source: %c\n\n  %i"
                          ":PROPERTIES:\n  :created: %u\n  :END:" "%^{content_tags}p")
                 "~/org/bookmarks.org" "Bookmarks")))
     
     (setq org-startup-truncated nil)

     ;; Information to record when a task moves to the DONE state.
     (setq org-log-done 'time)

     ;; Return activates link
     (setq org-return-follows-link t)

     ;; Hide leading stars
     (setq org-hide-leading-stars t)
     ;; http://orgmode.org/manual/Clean-view.html#Clean-view

     ;; Only use odd levels
     (setq org-odd-levels-only nil)

     ;; My todo levels
     (setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELED(c)")))

     ;; Sanitize C-a, C-e, C-k for org-mode
     (setq org-special-ctrl-a/e t)
     (setq org-special-ctrl-k t)

     ;; Never insert an empty line before a new entry -- if I want it I can add it
     ;; for myself.
     (setq org-blank-before-new-entry nil)

     ;; agenda
     (setq org-agenda-files (list "~/org/personal.org"
                                  "~/org/skola.org"))

     ;;; org-mode hooks
     (defun my-org-mode-hook-defun ()
       ;; make sure cua-mode is disabled
       (cua-mode -1)

       ;; flyspell unless this is my password file
       ;; (unless (string-equal (buffer-name) "secrets.org.gpg")
       ;;   (flyspell-mode 1))

       ;; read-only if this is my password file
       (when (string-equal (buffer-name) "secrets.org.gpg")
         (setq buffer-read-only t)))
     (add-hook 'org-mode-hook 'my-org-mode-hook-defun)

     ;;; publish
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

              :style "<link rel=\"stylesheet\"
                     href=\"style.css\"
                     type=\"text/css\"/>")))))

             ;; ("misc"
             ;;  :base-directory "~/misc/"
             ;;  :base-extension (regexp-opt '("asc" "css"))
             ;;  :publishing-directory "~/org/www/html"
             ;;  :publishing-function org-publish-attachment)))))

(provide 'my-org-mode)

;; my-org-mode.el ends here
