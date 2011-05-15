;;; git-to-svn-diff.el --- convert git patch to svn diff

;; Copyright (C) 2010, Stefan Kangas

;; Maintainer: Stefan Kangas
;; Keywords: git, subversion, vcs

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is mostly useful if you're using git-svn and wish to use the output of
;; git format-patch to send patches upstream.

;; This file depends on magit.el

;;; Change Log:

;;; Code:

;; TRACKING_BRANCH=`git config --get svn-remote.svn.fetch | sed -e 's/.*:refs\/remotes\///'`
;; REV=`git svn find-rev $(git rev-list --date-order --max-count=1 $TRACKING_BRANCH)`
;; git diff --no-prefix $(git rev-list --date-order --max-count=1 $TRACKING_BRANCH) $* |
;; sed -e "s/^+++ .*/& (working copy)/" -e "s/^--- .*/& (revision $REV)/" \
;; -e "s/^diff --git [^[:space:]]*/Index:/" \
;; -e "s/^index.*/===================================================================/"

(require 'magit)

(defun git-patch-to-svn-diff ()
  (interactive)
  (let* ((tracking-branch (magit-get "svn-remote.svn.fetch"))
         (svn-rev-commit (magit-git-string "rev-list" "--date-order"
                                           "--max-count=1" (or tracking-branch "")))
         (svn-rev-number (magit-git-string "svn" "find-rev" (or svn-rev-commit ""))))
    (delete-region (point-min)
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^---$")
                     (re-search-forward "^diff --git")
                     (beginning-of-line)
                     (point)))
    (save-excursion
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (re-search-forward "^--- a/\\(.*\\)" nil t)
          (replace-match (concat "--- \\1 (revision " svn-rev-number ")")))
        (goto-char (point-min))
        (while (re-search-forward "^+++ b/\\(.*\\)" nil t)
          (replace-match "+++ \\1 (working copy)"))
        (goto-char (point-min))
        (while (re-search-forward "^diff --git a/\\(.*\\) b/.*" nil t)
          (replace-match "Index: \\1"))
        (goto-char (point-min))
        (while (re-search-forward "^index.*" nil t)
          (replace-match "==================================================================="))))))
    
(provide 'git-to-svn-diff)

;;; git-to-svn-diff.el ends here
