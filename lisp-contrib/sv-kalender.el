;;; sv-kalender.el --- Swedish calendar for Emacs

;; Copyright (C) 2002,2003,2004,2007,2009,2018 Daniel Jensen

;; Author: Daniel Jensen <daniel@bigwalter.net>
;; Version: 1.9
;; Keywords: calendar swedish localization

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Kommentarer:

;; Swedish calendar localization. Note: Only a few comments in this
;; file are in English. The rest is in Swedish.

;; Denna fil modifierar hur Emacs kalender ser ut. Den byter namn p�
;; veckodagar, m�nader etc., samt inf�r svenska helgdagar och h�gtider
;; i st�llet f�r de amerikanska.
;;
;; F�r att anv�nda den svenska kalendern, spara filen i din load-path
;; och anv�nd (load "sv-kalender") i din ~/.emacs.

;;; History (�ndringar):

;; 1.9 - Update for Emacs 25
;; 1.8 - Emacs 23 support. GPLv3.
;; 1.6 - Lunar phase names, sunrise/sunset
;;       (m�nfasernas namn, soluppg�ng och -nedg�ng)
;; 1.5 - Cleanup, introduce sv prefix (st�dning, nytt sv-prefix)
;; 1.4 - Months and days use lower-case initials
;;       (m�nader och dagar med sm� begynnelsebokst�ver)
;; 1.3 - New flag days, Easter bug fixes
;;       (nya flaggdagar och en bugg i "mer p�sk" fixad) (Alan Campbell)
;; 1.2 - Advent Sundays fixed (adventsdagarna justerade) (Alan Campbell)
;; 1.1 - Fat Tuesday moved back a week (fettisdagen flyttad en vecka bak�t).

;;; Code:

;; Veckan b�rjar med en m�ndag
(setq calendar-week-start-day 1)

;; Anv�nd "europeiska" datum (dag/m�ndad)
(setq calendar-date-style 'european)

;; Datumformat
(setq  calendar-date-display-form
      '((if dayname
            (concat dayname ", "))
        day " " monthname " " year))

;; 24-timmarsklocka utan tidszon
(setq calendar-time-display-form
      '(24-hours ":" minutes))

;; Dagarnas namn
(setq calendar-day-name-array
      ["s�ndag" "m�ndag" "tisdag" "onsdag" "torsdag" "fredag" "l�rdag"])

;; M�nadernas namn
(setq calendar-month-name-array
      ["januari" "februari" "mars" "april" "maj" "juni"
       "juli" "augusti" "september" "oktober" "november" "december"])

;; M�nfaser
(defadvice lunar-phase-name (around sv-lunar-phase-name activate)
  "M�nfasernas namn p� svenska."
  (setq ad-return-value
	(let ((phase (ad-get-arg 0)))
	  (cond ((= 0 phase) "Nym�ne")
		((= 1 phase) "V�xande halvm�ne")
		((= 2 phase) "Fullm�ne")
		((= 3 phase) "Avtagande halvm�ne")))))

;; Soluppg�ng och -nedg�ng
(defadvice solar-sunrise-sunset-string (around sv-solar-sunrise-sunset-string
                                               activate)
  "Soluppg�ng och solnedg�ng p� svenska."
  (setq ad-return-value
        (let ((l (solar-sunrise-sunset date)))
          (format
           "%s, %s vid %s (%s timmar dagsljus)"
           (if (car l)
               (concat "Sol upp " (apply 'solar-time-string (car l)))
             "Ingen soluppg�ng")
           (if (car (cdr l))
               (concat "ned " (apply 'solar-time-string (car (cdr l))))
       "ingen solnedg�ng")
           (eval calendar-location-name)
           (car (cdr (cdr l)))))))

;; G�m vissa helgdagar?
(defvar sv-hide-some-holidays nil
  "Non-nil means some holidays won't show in the calendar.
Om icke-nil, g�m vissa helgdagar i kalendern.")

;; P�skdagen (from holiday-easter-etc)
(defun sv-easter (year)
  "Calculate the date for Easter in YEAR.
Ber�kna p�skdagen f�r �r YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

;; Helgdagar
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Ny�rsdagen")
        (holiday-fixed 1 6 "Trettondedag jul")

        ;; P�sk och pingst
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (sv-easter displayed-year) (car dag)))
                  (cadr dag)))
          '((  -2 "L�ngfredagen")
            (   0 "P�skdagen")
            (  +1 "Annandag p�sk")
            ( +39 "Kristi himmelf�rdsdag")
            ( +49 "Pingstdagen"))))

        (holiday-fixed 5 1 "F�rsta maj")

        (let ((midsommar-d (calendar-dayname-on-or-before
                            6 (calendar-absolute-from-gregorian
                               (list 6 26 displayed-year)))))
          ;; Midsommar
          (holiday-filter-visible-calendar
           (list
            (list
             (calendar-gregorian-from-absolute (1- midsommar-d))
             "Midsommarafton")
            (list
             (calendar-gregorian-from-absolute midsommar-d)
             "Midsommardagen")
            ;; Alla helgons dag
            (list
             (calendar-gregorian-from-absolute
              (calendar-dayname-on-or-before
               6 (calendar-absolute-from-gregorian
                  (list 11 6 displayed-year))))
             "Alla helgons dag"))))
        
        (holiday-fixed 12 25 "Juldagen")
        (holiday-fixed 12 26 "Annandag jul")))

;; Solst�nd, dagj�mningar, vinter- och sommartid
(setq holiday-solar-holidays
      (if sv-hide-some-holidays
          nil
        '((if (progn
                (require 'cal-dst)
                t)
              (funcall 'holiday-sexp calendar-daylight-savings-starts
                       '(format "Sommartid b�rjar %s"
                                (if
                                    (fboundp 'atan)
                                    (solar-time-string
                                     (/ calendar-daylight-savings-starts-time
                                        (float 60))
                                     calendar-standard-time-zone-name)
                                  ""))))
          (funcall 'holiday-sexp calendar-daylight-savings-ends
                   '(format "Vintertid b�rjar %s"
                            (if
                                (fboundp 'atan)
                                (solar-time-string
                                 (/ calendar-daylight-savings-ends-time
                                    (float 60))
                                 calendar-daylight-time-zone-name)
                              ""))))))

;; Listan med kalenderns helgdagar
(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays
              holiday-solar-holidays))

(provide 'sv-kalender)
