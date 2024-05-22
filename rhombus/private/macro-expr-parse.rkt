#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "treelist.rkt"
                     (submod "entry-point-adjustment.rkt" for-struct))
         syntax/parse/pre
         "pack.rkt"
         "macro-rhs.rkt"
         "macro-macro.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "parens.rkt")

(provide (for-syntax parse-macro-expression))

(define-for-syntax (parse-macro-expression stx adjustments)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (alts-tag::alts (_::block (group (_::quotes ((~and tag group) (_::parens) . pat))
                                               (~and rhs (_::block body ...))))
                              ...+))
     (expose-arity
      adjustments
      (parse-operator-definitions-rhs
       stx
       (parse-operator-definitions #'form-id
                                   'rule
                                   #:allowed '(prefix)
                                   stx
                                   (map no-srcloc (syntax->list #'((tag ignore . pat) ...)))
                                   (syntax->list #'(rhs ...))
                                   #f
                                   #f
                                   #f #f
                                   #'() #'() '())
       '#f
       #'wrap-prefix
       #f
       #f
       #:adjustments adjustments))]
    [(form-id (_::quotes ((~and tag group) (_::parens) . pat))
              (~and rhs (_::block body ...)))
     (expose-arity
      adjustments
      (parse-operator-definition-rhs
       stx
       (parse-operator-definition #'form-id
                                  'rule
                                  stx
                                  #:allowed '(prefix)
                                  (no-srcloc #'(tag ignore . pat))
                                  #'rhs
                                  #f
                                  #f)
       '#f
       #'wrap-prefix
       #f
       #:adjustments adjustments))]))

(define-for-syntax (expose-arity adjustments e)
  (wrap-static-info e
                    #'#%function-arity
                    #`(#,(+ 1 (treelist-length (entry-point-adjustment-prefix-arguments adjustments)))
                       ()
                       ())))

(define (wrap-prefix precedence protocol proc)
  (lambda (stx)
    (syntax-parse (unpack-tail stx #f #f)
      [(head . tail) (proc (pack-tail #'tail) #'head)])))
