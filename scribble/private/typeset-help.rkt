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

(define-for-syntax (resolve-name-ref root field)
  (define p (identifier-binding-portal-syntax root #f))
  (define lookup (and p (portal-syntax->lookup p (lambda (self-id lookup) lookup))))
  (define dest (and lookup (lookup #f "identifier" field)))
  (and dest
       (let ([raw (format "~a.~a"
                          (syntax-e root)
                          (syntax-e field))])
         (define loc-stx
           (append-consecutive-syntax-objects
            'loc-stx
            (append-consecutive-syntax-objects 'loc-stx root #'dot)
            field))
         (syntax-raw-property (datum->syntax dest (syntax-e dest) loc-stx loc-stx)
                              raw))))
