#lang racket/base
(require syntax/parse/pre
         (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "entry-point-adjustment.rkt")
         "macro-rhs.rkt"
         "entry-point.rkt"
         "pack.rkt")

(provide class-transformer)

(define-syntax (class-transformer stx)
  (syntax-parse stx
    [(_ #:single stx rhs)
     (parse-operator-definition-rhs
      #'stx
      #'rhs
      '#f
      #'wrap-prefix
      #f
      #:adjustments no-adjustments)]
    [(_ #:multi stx rhss)
     (parse-operator-definitions-rhs
      #'stx
      (syntax->list #'rhss)
      '#f
      #'wrap-prefix
      #f
      #f
      #:adjustments no-adjustments)]
    [(_ g)
     #:with (~var lam (:entry-point no-adjustments)) #'g
     #`lam.parsed]))

(define (wrap-prefix order precedence protocol proc)
  (lambda (stx)
    (syntax-parse (unpack-tail stx #f #f)
      [(head . tail) (proc (pack-tail #'tail) #'head)])))
