#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "parse.rkt"
                     "pack.rkt")
         "definition.rkt"
         "parsed.rkt"
         "parse.rkt"
         "parens.rkt"
         (submod "equal.rkt" for-parse))

(provide sequence_macro
         (for-syntax make_sequence_constructor))

(define-syntax sequence_macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ id (tag::block rhs-g))
         (list
          #'(define-sequence-syntax id
              (lambda () #'values)
              (rhombus-expression rhs-g)))]))))

(begin-for-syntax
  (define-syntax-class :identifiers
    #:attributes ([id 1])
    #:datum-literals (group)
    (pattern idx:identifier
             #:attr [id 1] (list #'idx))
    (pattern (_::parens (group idx:identifier) ...)
             #:attr [id 1] (syntax->list #'(idx ...)))))

(define-for-syntax (make_sequence_constructor proc)
  (lambda (stx)
    (syntax-parse stx
      [[(lhs:identifier ...) (_ expr)]
       (define s (proc #`(group lhs ... (block (group #,(parsed #'expr))))))
       (cond
         [s
          (syntax-parse s
            #:datum-literals (group)
            [(_::parens
              (group (_::parens
                      (group outer-ids::identifiers _::equal outer-rhs ...)
                      ...))
              outer-check-g
              (group (_::parens
                      (group loop-id:identifier _::equal loop-rhs ...)
                      ...))
              pos-guard-g
              (group (_::parens
                      (group inner-ids::identifiers _::equal inner-rhs ...)
                      ...))
              pre-guard-g
              post-guard-g
              (group (_::parens
                      recur-g
                      ...)))
             #'[(lhs ...)
                (:do-in
                 ([(outer-ids.id ...) (rhombus-expression (group outer-rhs ...))] ...)
                 (rhombus-expression outer-check-g)
                 ([loop-id (rhombus-expression (group loop-rhs ...))] ...)
                 (rhombus-expression pos-guard-g)
                 ([(inner-ids.id ...) (rhombus-expression (group inner-rhs ...))] ...)
                 (rhombus-expression pre-guard-g)
                 (rhombus-expression post-guard-g)
                 ((rhombus-expression recur-g) ...))]])]
         [else #f])])))
