;;; ffap-perl-module.el --- find perl module at point with ffap

;; Copyright 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 10
;; Keywords: files
;; URL: http://user42.tuxfamily.org/ffap-perl-module/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-perl-module.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-perl-module.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code lets M-x ffap find the source file for a perl module.
;; For example Foo::Bar becomes /usr/share/perl5/Foo/Bar.pm or wherever is
;; in the path.
;;
;; Variable names or subpackages are stripped, and a prefix is added if
;; unique.  See the `ffap-perl-module-file-at-point' docstring below for
;; details.
;;
;; The lookup is independent of the major mode, so you can be in Man-mode,
;; diff-mode, pod-mode or whatever and still go to perl source.

;;; Install:

;; Put ffap-perl-module.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-perl-module))
;;

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - allow point at start of "use", "no" or "require" statement
;;           - prefer "Changes" as file, not DBI/Changes.pm
;; Version 3 - use [:alpha:] etc to match non-ascii variable names
;; Version 4 - cope with non-existent `default-directory'
;; Version 5 - fix for unqualified variables $FOO
;; Version 6 - drop the enabling cookie as it can be too intrusive by default
;; Version 7 - use pipe rather than pty for subprocess
;; Version 8 - prefer file at point over search for perl module
;; Version 9 - set ffap-string-at-point variable
;; Version 10 - undo defadvice on unload-feature

;;; Code:

;;;###autoload
(defcustom ffap-perl-module-path nil
  "List of directories to search for perl modules.
If nil then function `ffap-perl-module-path' initializes it from
Perl's @INC when you first attempt an `ffap' perl module lookup."
  :type  '(repeat directory)
  :group 'ffap
  :link '(url-link
          :tag "ffap-perl-module.el home page"
          "http://user42.tuxfamily.org/ffap-perl-module/index.html"))

(defadvice ffap-string-at-point (around ffap-perl-module activate)
  "Extract a perl module filename at point.
See `ffap-perl-module-file-at-point' for details."

  ;; The expand-prefix stuff is a bit slow, so only run for mode==nil, not
  ;; url, machine, etc.  For the same reason prefer a filename at point over
  ;; a search, in particular this stops "Makefile" at point from churning
  ;; all the perl dirs when it's a filename rather than a module.
  ;;
  (unless (and (not mode)
               (not (let ((filename (ffap-string-at-point 'file)))
                      (and filename
                           (or (ffap-file-remote-p filename)
                               (file-exists-p filename)))))
               (let ((filename (ffap-perl-module-file-at-point)))
                 (and filename
                      (progn
                        (set-text-properties 0 (length filename) nil filename)
                        (setq ad-return-value
                              (setq ffap-string-at-point filename))))))
    ad-do-it))

(defun ffap-perl-module-unload-function ()
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-perl-module)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-perl-module)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(defun ffap-perl-module-path ()
  "Return a list of directory names to search for perl modules.
This function returns variable `ffap-perl-module-path' if it's not nil,
or initializes that by running \"perl -e print @INC\" for the
places perl will look, which is usually various /usr/share, /usr/local,
and whatever your PERL5LIB says.

The current directory \".\" which is normally in @INC is
deliberately excluded from the default calculation.  It's a bit
of a security hole and too easily makes `ffap-perl-module-expand-prefix'
churn deep through irrelevant directories."

  (or ffap-perl-module-path
      (with-temp-buffer
        (let ((default-directory "/") ;; in case inherit a non-existent
              (coding-system-for-read file-name-coding-system)
              (process-connection-type nil)) ;; pipe
          (call-process "perl" nil t nil "-e" "$,='\n'; print @INC"))
        (setq ffap-perl-module-path
              (or (remove "." (split-string (buffer-string) "\n"))
                  ;; something non-empty as a fallback
                  '("/usr/share/perl"))))))

;; No [:alnum:] etc in xemacs21, fallback to A-Z etc.  Which means unicode
;; in variable names doesn't match there, you have to have point on the
;; (ascii) package name part.  What would be an easy better way?
(eval-and-compile
  (let* ((alpha (if (string-match "[[:alpha:]]" "A") "[:alpha:]" "A-Za-z0-9"))
         (alnum (if (string-match "[[:alnum:]]" "A") "[:alnum:]" "A-Za-z0-9"))
         (word  (concat "[" alpha "_][" alnum "_]*")))

    (defconst ffap-perl-module-directory-regexp
      (concat "\\`" word "\\'")
      "Regexp for a directory name for packages.
This matches only a single word like \"Moose\" without any \"/\"s
etc.  It doesn't match .pm files to save some stat()s, and
doesn't match . or .. to avoid an infinite loop searching!

\[:alpha:] and [:alnum:] are used when available to maybe allow
unicode in package names, if you're brave enough to have
filenames in unicode.  A-Z fallbacks are used for xemacs21.")

    (defconst ffap-perl-module-qualif-regexp
      (concat "\\(" word "\\(" "::" word "\\)*\\)")
      "Regexp for a name with optional :: qualifiers.
This matches for instance \"FindBin\" or \"Moose::Util::something\".

\[:alpha:] and [:alnum:] are used when available to allow unicode
in variable names, and even in the package names (caveats as per
`ffap-perl-module-directory-regexp').  A-Z fallbacks are used for
xemacs21.")))

(defun ffap-perl-module-file-at-point ()
  "Find the filename for a perl module at point.
For example with point on Foo::Bar the return could be
\"/usr/share/perl5/Foo/Bar.pm\".  If there's nothing in
`ffap-perl-module-path' for a package at point then the return is
nil.

* $Foo::Bar::QUUX, &{Foo::Bar::QUUX} etc are recognised as
  variable or subroutine names and the package part is Foo::Bar.

  Currently this isn't applied to a plain calls Foo::Bar::func(),
  but they're pruned by the following rule so normally works ok.

* Foo::Bar::Quux is pruned back to Foo::Bar, or just Foo, if the
  full package doesn't exist.  This is good if a single file
  defines a set of sub-packages, or if Quux is actually a
  constant subr, etc.  It hopefully gets you close to the right
  package at least.

* Client::DNS or similar shorthand is expanded to say
  POE::Component::Client::DNS if that's the only Client::DNS.
  This is good in documentation where a long package prefix might
  be omitted, eg. in POE or Perl::Critic.

  The search for this may take a few of seconds depending how
  much is in your `ffap-perl-module-path' and its subdirectories.

* A single word at point without any \"::\", like say Symbol,
  will go to Symbol.pm for the few top-level perl modules.  But a
  leading or trailing / or . is taken to mean a filename, not a
  package name, and the return is nil in that case.  The latter
  prevents say the \"sort\" of \"sort.el\" offering sort.pm.

* If there's no .pm file for the package but there's a .pod then
  that's returned.  This is good for pseudo-packages like
  Module::Build::Cookbook which are just documentation.

* PoCo is recognised as an abbreviation for POE::Component.  It's
  found in documentation but the code is always the full name.

* Non-ascii variable names work fine in Emacs, but are not
  matched in XEmacs21 (ensure point is on the package name part
  instead).

  Non-ascii package names are matched (in Emacs), but it's up to
  you to ensure perl \"use utf8\", and that your locale, and
  emacs `file-name-coding-system', and the actual bytes in the
  name on disk, all coincide.  That may be asking for trouble
  most of the time! :-)

This function is designed for use under `ffap' so it sets
`ffap-string-at-point-region' to the part of the buffer
identified as the package name.

The ffap-perl-module.el home page is
URL `http://user42.tuxfamily.org/ffap-perl-module/index.html'"

  (eval-and-compile ;; quieten the byte compiler too
    (require 'ffap)
    (require 'thingatpt))

  (save-excursion
    ;; If point is just after a word then go back one char to let
    ;; thing-at-point-looking-at match on that previous word.  Is there a
    ;; more general way to get this effect?
    (and (not (bobp))
         (save-excursion
           (goto-char (1- (point)))
           (looking-at "\\S-\\(\\s-\\|\\'\\)"))
         (goto-char (1- (point))))

    ;; IN-USE-P is non-nil if we're looking at a "use", "no" or "require"
    (let* ((case-fold-search nil)
           (type nil))

      ;; `thing-at-point-looking-at' doesn't work well on a pattern with
      ;; optional variable length prefix like say "\\(@\\s-*\\)?foo".  It's
      ;; fine when you're on the @ of "@ foo", but if you're on the foo then
      ;; the back-up char-by-char fails to match the space in between and
      ;; stops there, without seeing there's an earlier @ which would
      ;; succeed.  The way `re-search-backward' won't match across its start
      ;; point is the basic culprit; the gambits in
      ;; `thing-at-point-looking-at' to get around that don't cope with a
      ;; multiple-char optional prefix.
      ;;
      ;; So the strategy is to try a variable name, variable name in {}, a
      ;; use/no/require, then a bare word.  The backslashes "\" in the
      ;; variables mean you can have point on a ref like \&foo and get foo
      ;; matched.  There's nothing else done with the fact it's making a
      ;; ref, it's just for point at the start of such a form.
      ;;
      (and (or (and (thing-at-point-looking-at
                     (concat "\\\\*[$@%&]\\s-*"
                             ffap-perl-module-qualif-regexp))
                    (setq type 'variable))

               (and (thing-at-point-looking-at
                     (concat "\\\\*[$@%&]\\s-*{\\s-*"
                             ffap-perl-module-qualif-regexp
                             "\\s-*}"))
                    (setq type 'variable))

               (and (thing-at-point-looking-at
                     (concat "\\(?:use\\|no\\|require\\)\\s-+"
                             ffap-perl-module-qualif-regexp))
                    (setq type 'use))

               (thing-at-point-looking-at ffap-perl-module-qualif-regexp))

           ;; don't chase down a bare word "Changes", prefer a normal ffap
           ;; of a file called Changes in the local directory instead of
           ;; DBI/Changes.pm which is the DBI package news file (it's a bit
           ;; nasty hard coding an exception like this, but it gets the
           ;; right effect ... maybe some other common words shouldn't be
           ;; chased too)
           (or type ;; but "use Changes" or "$Changes" is ok to keep going
               (not (equal (match-string 1) "Changes")))

           ;; leading or trailing / or . on a single word means a filename
           (or type ;; "use Foo." or "$Foo." is ok to keep going
               (match-beginning 2) ;; match 2 means multi-word, is ok
               (and (not (memq (char-before (match-beginning 0)) '(?/ ?.)))
                    (not (memq (char-after (match-end 0)) '(?/ ?.)))))

           ;; functions and variables $FOO or &foo must have at least one ::
           ;; qualifier for the package part
           (or (not (eq type 'variable))
               (match-beginning 2))

           ;; for variables etc beginning $ @ % & strip the variable name to
           ;; get the package part
           (setq ffap-string-at-point-region
                 (list (match-beginning 1)
                       (if (eq type 'variable)
                           (match-beginning 2) ;; before last "::foo"
                         (match-end 1))))

           (let* ((modname (apply 'buffer-substring
                                  ffap-string-at-point-region))
                  (basename (ffap-perl-module-modname-to-filename modname)))

             (when (string-match "\\`PoCo\\(::.*\\)?\\'" modname)
               (setq modname (concat "POE::Component"
                                     (match-string 1 modname))))

             ;; prefer .pm over .pod, even if .pod is earlier in the path
             (or (ffap-locate-file basename '(".pm") (ffap-perl-module-path))
                 (ffap-locate-file basename '(".pod") (ffap-perl-module-path))

                 ;; if there's no exact match then try the prefix business (but
                 ;; not on variables), then suffix pruning
                 (and (not (eq type 'variable))
                      (ffap-perl-module-expand-prefix modname))

                 (ffap-perl-module-prune-suffix modname)))))))

(defun ffap-perl-module-expand-prefix (modname)
  "Try to find MODNAME by putting a package prefix on it.
This some internals of `ffap-perl-module-file-at-point'.

MODNAME like \"Foo::Bar\" is sought with some prefix on it, like
\"Xyzzy::Foo::Bar\".  This is done by traversing all directories
and subdirectories of `ffap-perl-module-path', which might take a
few seconds if you've got a lot of stuff.

If there's a single such expanded package name then the filename
is returned, if not nil is returned.  If there's no .pm files at
all for MODNAME, then .pod is sought instead with the same
rules."

  (eval-and-compile (require 'cl))
  (catch 'stop
    (let* ((basename     (ffap-perl-module-modname-to-filename modname))
           (pm-basename  (concat basename ".pm"))
           (pod-basename (concat basename ".pm"))
           default-directory
           found-pod-pkg found-pod-filename
           found-pm-pkg  found-pm-filename)
      (dolist (pathdir (ffap-perl-module-path))
        ;; DIRLIST is absolute paths of directories to contemplate.  An
        ;; entry is taken off to inspect and its subdirectories are pushed
        ;; on, until no further directories and subdirectories exist.
        (let ((dirlist (condition-case nil
                           (directory-files
                            pathdir t ffap-perl-module-directory-regexp
                            t)  ;; no sort
                         (error nil))))
          ;; toplevel "auto" only has AutoSplit .al files
          ;; toplevel "LocaleData" only has .mo files for Locale::TextDomain
          ;; exclude these to shorten the search
          (setq dirlist (remove* "/\\(auto\\|LocaleData\\)\\'"
                                 dirlist :test 'string-match))
          (while dirlist
            ;; A few attempts (in emacs22) had it faster to set
            ;; default-directory and let directory-files give absolute
            ;; filenames, rather than concat directory and basename in lisp.
            ;; Ditto faster to stay iterative style with a DIRLIST than
            ;; recursive function calls to traverse.
            ;;
            ;; `file-name-as-directory' to ensure trailing slash,
            ;; directory-files gives the name only.
            ;;
            (setq default-directory (file-name-as-directory (car dirlist)))
            (setq dirlist (cdr dirlist))

            ;; diagnostic for what's traversed ...
            ;; (let ((dir default-directory))
            ;;   (with-current-buffer (get-buffer-create "x")
            ;;     (insert dir "\n")))

            (when (file-exists-p pm-basename)
              ;; pkg is without lib path part, eg. "/POE/Component/Client/",
              ;; just used to check uniqueness, ignoring shadowed copies
              ;; under different `pathdir's
              (let ((pkg (substring default-directory (length pathdir))))
                (if found-pm-pkg
                    (unless (equal pkg found-pm-pkg)
                      (throw 'stop nil)) ;; not unique, no good
                  (setq found-pm-pkg pkg)
                  (setq found-pm-filename
                        (concat default-directory pm-basename)))))

            (when (file-exists-p pod-basename)
              (let ((pkg (substring default-directory (length pathdir))))
                (if found-pod-pkg
                    (setq found-pod-filename nil) ;; not unique, discard
                  (setq found-pod-pkg pkg)
                  (setq found-pod-filename
                        (concat default-directory pod-basename)))))

            ;; Appending subdirs to dirlist means breadth-first traversal.
            ;; That might have a slightly better chance of seeing a
            ;; duplicate package name for some generic kind of word.  But a
            ;; successful search must traverse everything, so the order
            ;; doesn't matter all that much.
            (setq dirlist (nconc dirlist
                                 (condition-case nil
                                     (directory-files
                                      default-directory t
                                      ffap-perl-module-directory-regexp
                                      t)  ;; no sort
                                   (error nil)))))))
      (or found-pm-filename
          found-pod-filename))))
          
(defun ffap-perl-module-prune-suffix (modname)
  "Try to match MODNAME with suffix parts pruned off.
This some internals of `ffap-perl-module-file-at-point.

MODNAME like \"Aaa::Bbb::Ccc::Ddd\" is looked up shortened first
to \"Aaa::Bbb::Ccc\" then \"Aaa::Bbb\" and finally \"Aaa\".  The
return is a filename string, or nil if not found.  At each
pruning level .pm is tried then .pod.

If found then the endpoint in `ffap-string-at-point-region' is
shortened according to how much was pruned off MODNAME."

  (let ((orig modname)
        (path (ffap-perl-module-path)))
    (catch 'stop
      (while modname
        (let* ((basename (ffap-perl-module-modname-to-filename modname))
               ;; prefer .pm over .pod, even if .pod is earlier in the path
               (filename (or (ffap-locate-file basename '(".pm") path)
                             (ffap-locate-file basename '(".pod") path))))

          (when filename
            ;; found, adjust region for how much trimmed to make MODNAME;
            ;; `max' not to be before the start point in case something
            ;; freaky has happened pruning an expanded abbreviation back
            ;; beyond the expansion ...
            (setq ffap-string-at-point-region
                  (list (car ffap-string-at-point-region)
                        (max (car ffap-string-at-point-region)
                             (- (cadr ffap-string-at-point-region)
                                (- (length orig) (length modname))))))
            (throw 'stop filename))

          ;; strip last so Foo::Bar::Quux becomes Foo::Bar, or nil when no
          ;; more "::"s
          (setq modname (and (string-match "\\(.*\\)::" modname)
                             (match-string 1 modname))))))))

(defun ffap-perl-module-modname-to-filename (modname)
  "Return a filename for perl module MODNAME.
MODNAME is a string like \"Foo::Bar::Quux\", the return simply
has each \"::\" turned into \"/\" like \"Foo/Bar/Quux\"."
  (mapconcat 'identity (split-string modname ":+") "/"))

(provide 'ffap-perl-module)

;;; ffap-perl-module.el ends here
