;;; enter.rkt -- custom module loaders

;; Copyright (C) 2010 Jose Antonio Ortega Ruiz

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

;; Start date: Wed Mar 31, 2010 21:53

#lang racket/base

(require syntax/modcode
         (for-syntax racket/base)
         racket/path)

(provide get-namespace enter-module module-loader module-loaded?)

(struct mod (name load-path timestamp depends))

(define loaded (make-hash))

(define (module-loaded? path)
  (with-handlers ([exn? (lambda (_) #f)])
    (let ([rp (module-path-index-resolve (module-path-index-join path #f))])
      (hash-has-key? loaded (resolved-module-path-name rp)))))

(define (enter-module mod)
  (dynamic-require mod #f)
  (check-latest mod))

(define (module-loader orig)
  (enter-load/use-compiled orig #f))

(define (notify re? path)
  (when re?
    (fprintf (current-error-port) " [re-loading ~a]\n" path)))

(define inhibit-eval (make-parameter #f))

(define (get-namespace mod)
  (let ([mod (cond [(symbol? mod) mod]
                   [(string? mod) (find-module! (string->path mod) mod)]
                   [(path? mod) (find-module! mod (path->string mod))]
                   [else mod])])
    (and mod
         (with-handlers ([exn? (lambda (_) #f)])
           (parameterize ([inhibit-eval #t])
             (module->namespace mod))))))

(define (find-module! path path-str)
  (let ([m (or (hash-ref loaded path #f)
               (let loop ([ps (remove path (resolve-paths path))]
                          [seen '()])
                 (cond [(null? ps) #f]
                       [(hash-ref loaded (car ps) #f) =>
                        (lambda (m)
                          (add-paths! m (cdr ps))
                          (add-paths! m (cons path seen))
                          m)]
                       [else (loop (cdr ps) (cons (car ps) seen))])))])
    (list 'file (or (and m (mod-load-path m)) path-str))))

(define (add-paths! m ps)
  (for-each (lambda (p) (hash-set! loaded p m)) ps))

(define (resolve-paths path)
  (define (find root rest)
    (let* ([alt-root (resolve-path root)]
           [same? (equal? root alt-root)])
      (cond [(null? rest) (cons root (if same? '() `(,alt-root)))]
            [else (let* ([c (car rest)]
                         [cs (cdr rest)]
                         [rps (find (build-path root c) cs)])
                    (if same?
                        rps
                        (append rps (find (build-path alt-root c) cs))))])))
  (let ([cmps (explode-path path)])
    (find (car cmps) (cdr cmps))))

(define ((enter-load/use-compiled orig re?) path name)
  (when (inhibit-eval)
    (raise (make-exn:fail "namespace not found"
                          (current-continuation-marks))))
  (if name
      ;; Module load:
      (let ([code (get-module-code path "compiled" compile
                                   (lambda (ext loader?)
                                     (load-extension ext)
                                     #f)
                                   #:notify (lambda (chosen)
                                              (notify re? chosen)))]
            [path (normal-case-path
                   (simplify-path
                    (path->complete-path path
                                         (or (current-load-relative-directory)
                                             (current-directory)))))])
        ;; Record module timestamp and dependencies:
        (let ([m (mod name
                      (path->string path)
                      (get-timestamp path)
                      (if code
                          (apply append
                                 (map cdr
                                      (module-compiled-imports code)))
                          null))])
          (add-paths! m (resolve-paths path)))
        ;; Evaluate the module:
        (eval code))
      ;; Not a module:
      (begin
        (notify re? path)
        (orig path name))))

(define (get-timestamp path)
  (file-or-directory-modify-seconds path #f (lambda () -inf.0)))

(define (check-latest mod)
  (let ([mpi (module-path-index-join mod #f)]
        [done (make-hash)])
    (let loop ([mpi mpi])
      (let* ([rpath (module-path-index-resolve mpi)]
             [path (resolved-module-path-name rpath)])
        (when (path? path)
	  (let ([path (normal-case-path path)])
            (unless (hash-ref done path #f)
              (hash-set! done path #t)
              (let ([mod (hash-ref loaded path #f)])
                (when mod
                  (for-each loop (mod-depends mod))
                  (let ([ts (get-timestamp path)])
                    (when (ts . > . (mod-timestamp mod))
                      (let ([orig (current-load/use-compiled)])
                        (parameterize ([current-load/use-compiled
                                        (enter-load/use-compiled orig #f)]
                                       [current-module-declare-name rpath])
                          ((enter-load/use-compiled orig #t)
                           path
                           (mod-name mod)))))))))))))))

;;; enter.rkt ends here
