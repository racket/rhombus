#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (for-syntax racket/base
                                 syntax/parse/pre)
                     "macro-rhs.rkt"
                     "srcloc.rkt"
                     "tag.rkt")
         syntax/parse/pre
         "macro-rhs.rkt"
         "definition.rkt"
         "expression.rkt"
         "entry-point.rkt"
         "macro-macro.rkt"
         "pack.rkt"
         "parse.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "parens.rkt"
         (submod "expr-macro.rkt" for-define))

(provide macro)

(begin-for-syntax
  (struct definition+entry-point+expression-transformer (def cbl exp)
    #:property prop:definition-transformer (lambda (self) (definition+entry-point+expression-transformer-def self))
    #:property prop:entry-point-transformer (lambda (self) (definition+entry-point+expression-transformer-cbl self))
    #:property prop:expression-prefix-operator (lambda (self) (definition+entry-point+expression-transformer-exp self)))
  (define (make-definition+entry-point+expression-transformer def cbl exp)
    (definition+entry-point+expression-transformer def cbl exp)))

(define-for-syntax (parse-macro stx adjustments)
  (syntax-parse stx
    #:datum-literals (parens group block alts op)
    [(form-id ((~and alts-tag alts) (block (group (_::quotes ((~and tag group) (_::parens) . pat))
                                                  (~and rhs (block body ...))))
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
                                   (lambda (x) x)
                                   #f)
       #'wrap-prefix
       #f
       #f
       #:adjustments adjustments))]
    [(form-id (_::quotes ((~and tag group) (_::parens) . pat))
              (~and rhs (block body ...)))
     (expose-arity
      adjustments
      (parse-operator-definition-rhs
       (parse-operator-definition #'form-id
                                  'rule
                                  #:allowed '(prefix)
                                  (no-srcloc #'(tag ignore . pat))
                                  #'rhs
                                  (lambda (x) x)
                                  #f)
       #'wrap-prefix
       #f
       #:adjustments adjustments))]))

(define-syntax macro
  (make-definition+entry-point+expression-transformer
   (definition-transformer
     (lambda (stx)
       (syntax-parse stx
         #:datum-literals (parens group block alts quotes op)
         [(form-id (_::alts (_::block (group (_::quotes (group) (_::parens) . pat) (_::block . _)))
                         ...+))
          ;; found () in place of a defined name, so parse as an expression
          #`((#%expression (rhombus-expression (#,group-tag . #,stx))))]
         [(form-id (_::quotes (group (_::parens) . _))
                   (_::block . _))
          ;; another expression case
          #`((#%expression (rhombus-expression (#,group-tag . #,stx))))]
         [(form-id (_::alts (_::block (group q::operator-syntax-quote
                                             (~and rhs (_::block body ...))))
                            ...+))
          (list
           (parse-operator-definitions #'form-id
                                       'rule
                                       stx
                                       (syntax->list #'(q.g ...))
                                       (syntax->list #'(rhs ...))
                                       #f
                                       #'rules-rhs))]
         [(form-id q::operator-syntax-quote
                   (~and rhs (_::block body ...)))
          (list
           (parse-operator-definition #'form-id
                                      'rule
                                      #'q.g
                                      #'rhs
                                      #f
                                      #'rule-rhs))])))
   (entry-point-transformer
    ;; parse macro
    (lambda (stx adjustments)
      (parse-macro stx adjustments))
    ;; extract arity
    (lambda (stx)
      1))
   (expression-transformer
    #'macro
    (lambda (stx)
      (values (parse-macro stx no-adjustments)
              #'())))))

(define (wrap-prefix name precedence protocol proc)
  (lambda (stx)
    (syntax-parse (unpack-tail stx #f #f)
      [(head . tail) (proc (pack-tail #'tail) #'head)])))

(define-for-syntax (expose-arity adjustments e)
  (wrap-static-info e
                    #'#%function-arity
                    #`(#,(+ 1 (length (entry-point-adjustments-prefix-arguments adjustments)))
                       ()
                       ())))

(begin-for-syntax
  (define-syntax (rules-rhs stx)
    (syntax-parse stx
      [(_ orig-stx pre-parsed ...)
       (parse-operator-definitions-rhs #'orig-stx (syntax->list #'(pre-parsed ...))
                                       #'make-expression-prefix-operator
                                       #'make-expression-infix-operator
                                       #'expression-prefix+infix-operator)]))
  (define-syntax (rule-rhs stx)
    (syntax-parse stx
      [(_ pre-parsed)
       (parse-operator-definition-rhs #'pre-parsed
                                      #'make-expression-prefix-operator
                                      #'make-expression-infix-operator)])))
