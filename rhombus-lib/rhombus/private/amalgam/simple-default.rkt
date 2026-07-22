#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "origin.rkt"
                     "injected.rkt")
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parse.rkt"
         "parens.rkt"
         "expression.rkt"
         (only-in "apostrophe.rkt"
                  |#'|)
         (only-in "char.rkt"
                  Char)
         (only-in "byte.rkt"
                  Byte))

(provide (for-syntax parse-simple-default))

(module+ normal-literal
  (provide (for-syntax normal-literal?
                       normal-literal-id?
                       install-normal-literal?!)))

;; NOTE The following condition is borrowed from
;; `racket/private/immediate-default`.  We need to recognize exactly
;; what it recognizes, because Rhombus `fun`s ultimately expand into
;; Racket `lambda`s.

(define-for-syntax small-string-threshold 8)

(define-for-syntax (simple-literal? v)
  (or (boolean? v)
      (number? v)
      (char? v)
      (and (string? v)
           ((string-length v) . < . small-string-threshold))
      (and (bytes? v)
           ((bytes-length v) . < . small-string-threshold))
      (and (hash? v)
           (eqv? (hash-count v) 0))))

(define-for-syntax (parse-simple-default stx)
  (define inner-g
    (syntax-parse stx
      #:datum-literals (group parsed)
      #:literals (rhombus-body-at)
      [(group (parsed #:rhombus/expr (rhombus-body-at _::block g))) #'g]
      [_ stx]))
  (define-values (parsed-e form-id)
    (syntax-parse inner-g
      #:datum-literals (group)
      [(group datum)
       (define v (syntax-e (uninject #'datum)))
       (if (and (simple-literal? v)
                (normal-literal? #'datum))
           (values (quote-implicit-literal #f #'datum)
                   (datum->syntax #'datum '#%literal))
           (values #f #f))]
      [(group . (~var name (:hier-name-seq in-name-root-space in-expression-space name-path-op name-root-ref)))
       #:with (datum) #'name.tail
       (define v (syntax-e (uninject #'datum)))
       (cond
         [(or (and (simple-literal? v)
                   (normal-literal-id? #'name.name)
                   (quote-implicit-literal #'name.name #'datum))
              (let ([inner (or (and (symbol? v)
                                    (or (symbol-interned? v)
                                        (symbol-unreadable? v))
                                    (free-identifier=? #'name.name
                                                       (expr-quote |#'|))
                                    #'datum)
                               (and (string? v)
                                    (eqv? (string-length v) 1)
                                    (free-identifier=? #'name.name
                                                       (expr-quote Char))
                                    (string-ref v 0))
                               (and (bytes? v)
                                    (eqv? (bytes-length v) 1)
                                    (free-identifier=? #'name.name
                                                       (expr-quote Byte))
                                    (bytes-ref v 0)))])
                (and inner
                     (relocate+reraw
                      (respan (datum->syntax #f (list #'name.name #'datum)))
                      #`(quote #,inner)))))
          => (lambda (parsed-e)
               (values parsed-e #'name.name))]
         [else (values #f #f)])]
      [_ (values #f #f)]))
  (and parsed-e
       (add-origin (syntax-local-introduce form-id) parsed-e)))

(define-for-syntax normal-literal? #f)
(define-for-syntax normal-literal-id? #f)
(define-for-syntax quote-implicit-literal #f)

(define-for-syntax (install-normal-literal?! proc id-proc quote-proc)
  (set! normal-literal? proc)
  (set! normal-literal-id? id-proc)
  (set! quote-implicit-literal quote-proc))
