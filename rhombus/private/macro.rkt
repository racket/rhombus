#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     (for-syntax racket/base
                                 syntax/parse/pre)
                     "macro-rhs.rkt"
                     "operator-parse.rkt"
                     "srcloc.rkt"
                     "tag.rkt")
         syntax/parse/pre
         "provide.rkt"
         "macro-expr-parse.rkt"
         "definition.rkt"
         "expression.rkt"
         "expression+definition.rkt"
         "entry-point.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "parens.rkt"
         (submod "expr-macro.rkt" for-define))

(provide (for-spaces (#f
                      rhombus/entry_point)
                     macro))

(define-syntax macro
  (make-expression+definition-transformer
   (expression-transformer
    (lambda (stx)
      (values (parse-macro-expression stx no-adjustments)
              #'())))
   (definition-transformer
     (lambda (stx)
       (syntax-parse stx
         #:datum-literals (group)
         [(form-id (_::alts (_::block (group (_::quotes (group (_::parens) . pat)) (_::block . _)))
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
                                       '#f
                                       #'rules-rhs
                                       #f #f
                                       #'() #'()))]
         [(form-id main-op::operator-or-identifier
                   (~optional (_::block
                               (~var main-options (:all-operator-options #f))))
                   (_::alts ~!
                            (_::block (group q::operator-syntax-quote
                                             (~and rhs (_::block body ...))))
                            ...+))
          (list
           (parse-operator-definitions #'form-id
                                       'rule
                                       stx
                                       (syntax->list #'(q.g ...))
                                       (syntax->list #'(rhs ...))
                                       '#f
                                       #'rules-rhs
                                       (if (attribute main-op) #'main-op.name #'#f)
                                       #'#f
                                       (if (attribute main-options) #'main-options.prec #'())
                                       (if (attribute main-options) #'main-options.assc #'())))]
         [(form-id q::operator-syntax-quote
                   (~and rhs (_::block body ...)))
          (list
           (parse-operator-definition #'form-id
                                      'rule
                                      stx
                                      #'q.g
                                      #'rhs
                                      '#f
                                      #'rule-rhs))])))))

(define-entry-point-syntax macro
  (entry-point-transformer
   ;; parse macro
   (lambda (stx adjustments)
     (parse-macro-expression stx adjustments))
   ;; extract arity
   (lambda (stx)
     1)))

(begin-for-syntax
  (define-syntax (rules-rhs stx)
    (syntax-parse stx
      [(_ #:multi orig-stx pre-parsed ...)
       (parse-operator-definitions-rhs #'orig-stx (syntax->list #'(pre-parsed ...))
                                       '#f
                                       #'make-expression-prefix-operator
                                       #'make-expression-infix-operator
                                       #'expression-prefix+infix-operator)]))
  (define-syntax (rule-rhs stx)
    (syntax-parse stx
      [(_ #:single orig-stx pre-parsed)
       (parse-operator-definition-rhs #'orig-stx #'pre-parsed
                                      '#f
                                      #'make-expression-prefix-operator
                                      #'make-expression-infix-operator)])))
