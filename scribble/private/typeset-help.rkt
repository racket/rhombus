#lang racket/base
(require (for-syntax racket/base
                     shrubbery/property
                     racket/syntax-srcloc)
         (only-in rhombus/private/name-root-ref
                  portal-syntax->lookup))

(provide (for-syntax
          resolve-name-ref
          append-consecutive-syntax-objects))

(define-for-syntax (append-consecutive-syntax-objects datum pre t)
  (define pre-loc (syntax-srcloc pre))
  (define t-loc (syntax-srcloc pre))
  (define t/s (if (and pre-loc
                       t-loc
                       (equal? (srcloc-source pre-loc)
                               (srcloc-source t-loc))
                       (srcloc-position t-loc)
                       (srcloc-span t-loc)
                       (srcloc-position pre-loc)
                       ((srcloc-position pre-loc) . < . (srcloc-position t-loc)))
                  (datum->syntax t
                                 datum
                                 (struct-copy srcloc pre-loc
                                              [span (- (+ (srcloc-position t-loc)
                                                          (srcloc-span t-loc))
                                                       (srcloc-position pre-loc))])
                                 t
                                 t)
                  t))
  (syntax-raw-prefix-property t/s (syntax-raw-prefix-property pre)))

(define-for-syntax (resolve-name-ref space-name root fields)
  (cond
    [(null? fields) #f]
    [else
     (define field (car fields))
     (define p (identifier-binding-portal-syntax root #f))
     (define lookup (and p (portal-syntax->lookup p (lambda (self-id lookup) lookup))))
     (define intro (if space-name
                       (make-interned-syntax-introducer space-name)
                       (lambda (x) x)))
     (define dest (and lookup (lookup #f "identifier" field intro)))
     (define raw (format "~a.~a"
                         (or (syntax-raw-property root)
                             (syntax-e root))
                         (syntax-e field)))
     (define (add-rest p) (and p (cons p (cdr fields))))
     (cond
       [dest
        (define loc-stx
          (append-consecutive-syntax-objects
           'loc-stx
           (append-consecutive-syntax-objects 'loc-stx root #'dot)
           field))
        (define named-dest
          (syntax-raw-property (datum->syntax dest (syntax-e dest) loc-stx loc-stx)
                               raw))
        (or (resolve-name-ref space-name named-dest (cdr fields))
            (add-rest named-dest))]
       [else
        (define id (intro (datum->syntax root (string->symbol raw))))
        (and (identifier-binding id #f)
             (let ([named-id (syntax-raw-property
                              (datum->syntax id (syntax-e id)
                                             (append-consecutive-syntax-objects
                                              'loc-stx
                                              (append-consecutive-syntax-objects 'loc-stx root #'dot)
                                              field))
                              raw)])
               (or (resolve-name-ref space-name named-id (cdr fields))
                   (add-rest named-id))))])]))
