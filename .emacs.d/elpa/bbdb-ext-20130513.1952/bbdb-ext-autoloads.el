;;; bbdb-ext-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (bbdb-address bbdb-recursive-search-address bbdb-recursive-search-phones
;;;;;;  bbdb-recursive-search-notes bbdb-recursive-search-net bbdb-recursive-search-company
;;;;;;  bbdb-recursive-search-name bbdb-recursive-search bbdb-google-map)
;;;;;;  "bbdb-ext" "bbdb-ext.el" (20924 33898 445211 706000))
;;; Generated autoloads from bbdb-ext.el

(autoload 'bbdb-google-map "bbdb-ext" "\
Search REC's address field using Google Maps.
If REC is `nil', the current record will be used.
If there is no address filed for REC, a message will be given in minibuffer.
If there are several addresses for REC, the address nearest point will be used.

\(fn &optional REC)" t nil)

(autoload 'bbdb-recursive-search "bbdb-ext" "\
Display all entries in the *BBDB* buffer matching the REGEX in either the name(s), company, network address, or notes.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-name "bbdb-ext" "\
Display all entries in the *BBDB* buffer matching the REGEX in the name (or ``alternate'' names) field.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-company "bbdb-ext" "\
Display all entries in *BBDB* buffer matching REGEX in the company field.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-net "bbdb-ext" "\
Display all entries in *BBDB* buffer matching regexp REGEX in the network address.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-notes "bbdb-ext" "\
Display all entries in *BBDB* buffer matching REGEX in the named notes field.

\(fn WHICH REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-phones "bbdb-ext" "\
Display all entries in *BBDB* buffer matching the REGEX in the phones field.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-recursive-search-address "bbdb-ext" "\
Display all entries in the *BBDB* buffer matching the REGEX in the address fields.

\(fn REGEX ELIDEP)" t nil)

(autoload 'bbdb-address "bbdb-ext" "\
Display all entries in the BBDB matching the REGEX in the address field.

\(fn REGEX ELIDEP)" t nil)

;;;***

;;;### (autoloads nil nil ("bbdb-ext-pkg.el") (20924 33898 565266
;;;;;;  127000))

;;;***

(provide 'bbdb-ext-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bbdb-ext-autoloads.el ends here
