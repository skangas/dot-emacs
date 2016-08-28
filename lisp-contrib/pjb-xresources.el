;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pjb-xresources.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Produces ~/.Xresources for the current settings of emacs.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-02-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(defun symbol-name* (sym)
  (let* ((name (symbol-name sym))
         (colon (position (character ":") name)))
    (cond 
      ((and colon (char= (character ":") (char name (1+ colon))))
       (subseq name (+ 2 colon)))
      (colon
       (subseq name (+ 1 colon)))
      (t name))))


(defun current-frame ()
  "
RETURN: The current frame.
"
  (selected-frame))


(defmacro define-frame-parameter (name)
  `(defun ,(intern (format "frame-%s" name)) (&optional frame)
     (frame-parameter (or frame (selected-frame)) ',name)))

;; (dolist (p (frame-parameters)) (insert (format "(define-frame-parameter %s)\n" (car p))))

(define-frame-parameter parent-id)
(define-frame-parameter display)
(define-frame-parameter visibility)
(define-frame-parameter icon-name)
(define-frame-parameter outer-window-id)
(define-frame-parameter window-id)
(define-frame-parameter top)
(define-frame-parameter left)
(define-frame-parameter buffer-list)
(define-frame-parameter unsplittable)
(define-frame-parameter minibuffer)
(define-frame-parameter modeline)
(define-frame-parameter width)
(define-frame-parameter height)
(define-frame-parameter name)
(define-frame-parameter background-mode)
(define-frame-parameter display-type)
(define-frame-parameter horizontal-scroll-bars)
(define-frame-parameter scroll-bar-width)
(define-frame-parameter cursor-type)
(define-frame-parameter auto-lower)
(define-frame-parameter auto-raise)
(define-frame-parameter icon-type)
(define-frame-parameter wait-for-wm)
(define-frame-parameter title)
(define-frame-parameter buffer-predicate)
(define-frame-parameter tool-bar-lines)
(define-frame-parameter menu-bar-lines)
(define-frame-parameter scroll-bar-background)
(define-frame-parameter scroll-bar-foreground)
(define-frame-parameter right-fringe)
(define-frame-parameter left-fringe)
(define-frame-parameter line-spacing)
(define-frame-parameter screen-gamma)
(define-frame-parameter border-color)
(define-frame-parameter cursor-color)
(define-frame-parameter mouse-color)
(define-frame-parameter background-color)
(define-frame-parameter foreground-color)
(define-frame-parameter vertical-scroll-bars)
(define-frame-parameter internal-border-width)
(define-frame-parameter border-width)
(define-frame-parameter font)


(defalias 'frame-pixel-top  'frame-top)
(defalias 'frame-pixel-left 'frame-left)


(defun set-default-frame-parameter (name value)
  (let ((acell (assoc name default-frame-alist)))
    (if acell
        (setf (cdr acell) value)
        (push (cons name value)  default-frame-alist))
    value))


(defun frame-geometry (&optional frame)
  (let ((frame (or frame (current-frame))))
    (format "%dx%d-%d+%d"
            (frame-width frame) (frame-height frame)
            (frame-pixel-left frame) (frame-pixel-top frame))))

;; (defun frame-full-screen (&optional frame)
;;   (let* ((frame    (or frame (current-frame)))
;;          (fwidth   (frame-pixel-width))
;;          (fheight  (frame-pixel-height))
;;          (percent  0.95))
;;     (destructuring-bind (stop sleft sheight swidth) (screen-usable-area (current-frame))
;;       (flet ((within (percent a b)
;;                (message "%f %f %f %f" percent a b (* percent b))
;;                (and (<=  (* percent b) a) (<= a b))))
;;        (if (within percent fwidth swidth)
;;            (if (within percent fheight sheight)
;;                "fullboth"
;;                "fullwidth")
;;            (if (within percent fheight sheight)
;;                "fullheight"
;;                nil))))))




(defun on-off (generalized-boolean)
  (if generalized-boolean "on" "off"))

(defun or-unset (object)
  (or object :unset))

(defconst +space+ 32)

(defun insert-x-resource-line (width section key value)
  (insert
   (if (eq value :unset)
       (format "! %s%s:\n" section key)
       (format "%s%s: %s %s\n" section key (make-string (- width (length key)) +space+) value))))

(defun generate-x-resources* (section plist)
  (loop
     with width = (loop for (key value) on plist by (function cddr) maximize (length key))
     for (key value) on plist by (function cddr)
     do  (insert-x-resource-line width section  key (or-unset value))))

(defmacro generate-x-resources (&rest sections)
  `(progn
     ,@(mapcar (lambda (section)
                 `(generate-x-resources* ,(first section) 
                                         (list ,@(mapcan (function copy-list) (rest section)))))
               sections)))

(defun generate-x-resources-for-face (face)
  (loop
     with width = (loop for (attribute (key . rest)) in face-x-resources maximize (length key))
     for (attribute (key . rest)) in face-x-resources
     do (ignore-errors (insert-x-resource-line width face key
                                               (or-unset
                                                (let ((val (face-attribute face attribute t)))
                                                  (if (member attribute '(:box))
                                                      (and (not (eq 'unspecified val)) (on-off val))
                                                      (and (not (eq 'unspecified val)) val))))))))

(defun insert-x-resources ()
  (interactive)
  (generate-x-resources
   ("emacs"
    ("*background"     (frame-background-color))
    ("*bitmapIcon"     (on-off (frame-icon-name)))
    ("*borderColor"    (frame-border-color))
    ("*borderWidth"    (frame-border-width))
    ("*cursorColor"    (frame-cursor-color))
    ("*cursorBlink"    (on-off blink-cursor))
    ("*font"           (frame-font))
    ("*fontBackend"    :unset)
    ("*foreground"     (frame-foreground-color))
    ("*geometry"       (frame-geometry))
    ;;("*fullscreen"     (or-unset (frame-full-screen)))
    ("*iconName"       (or-unset (frame-icon-name)))
    ("*internalBorder" (frame-internal-border-width))
    ("*lineSpacing"    (or-unset (frame-line-spacing)))
    ("*menuBar"        (on-off menu-bar-mode))
    ("*minibuffer"     (if (frame-minibuffer) :unset "none"))
    ("*paneFont"       :unset) ; how to get it?    Font name for menu pane titles, in non-toolkit versions of Emacs.
    ("*pointerColor"   (frame-mouse-color))
    ("*privateColormap" :unset) ;  If ‘on’, use a private color map, in the case where the “default visual” of class PseudoColor and Emacs is using it.
    ("*reverseVideo"   (on-off (cdr (or (assq 'reverse (frame-parameters))
                                       (assq 'reverse default-frame-alist)))))
    ("*screenGamma"    (or-unset (frame-screen-gamma)))
    ("*scrollBarWidth" (frame-scroll-bar-width))
    ("*selectionFont" :unset) ;  Font name for pop-up menu items, in non-toolkit versions of Emacs. (For toolkit versions, see Lucid Resources, also see LessTif  Resources.)
    ("*selectionTimeout" :unset) ; Number of milliseconds to wait for a selection reply. If the selection owner doesn't reply in this time, we give up. A value of 0  means wait as long as necessary.
    ("*synchronous" :unset) ; Run Emacs in synchronous mode if ‘on’. Synchronous mode is useful for debugging X problems.
    ("*title"       (or (frame-title) (frame-name)))
    ;; ("*toolBar" (if top-toolbar
    ;;                top-toolbar-height
    ;;                0))
    ("*useXIM"  :unset) ; Turn off use of X input methods (XIM) if ‘false’ or ‘off’. This is only relevant if your Emacs is actually built with XIM support.  It is potentially useful to turn off XIM for efficiency, especially slow X client/server links.
    ("*verticalScrollBars" (on-off (frame-vertical-scroll-bars)))
    ("*visualClass"        (or-unset (x-display-visual-class))))
   ("emacs*menu"
    ("*font"              (face-font 'menu))
    ("*fontSet"           :unset) ; How to get it?  Fontset for menu item text.
    ("*foreground"        (face-foreground 'menu))
    ("*background"        (face-background 'menu))
    ("*buttonForeground"  (face-foreground 'button))
    ("*horizontalSpacing" (or 3))        ; How to get them?
    ("*verticalSpacing"   (or 2))
    ("*arrowSpacing"      (or 10))
    ("*shadowThickness"   (or 1))
    ("*margin"            (or 1))))
  (dolist (face (face-list))
    (generate-x-resources-for-face face)))

