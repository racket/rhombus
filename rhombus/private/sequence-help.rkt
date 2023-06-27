#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "parse.rkt"
                     "pack.rkt")
         "definition.rkt"
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
       (define s (proc #`(group lhs ... (block (group (parsed #:rhombus/expr expr))))))
       (cond
         [s
          (syntax-parse s
            #:datum-literals (group)
            [(_::parens
              (~optional (group
                          #:outer_binds
                          (_::block
                           (group outer-ids::identifiers _::equal outer-rhs ...)
                           ...))
                         #:defaults ([(outer-ids 1) '()]
                                     [(outer-rhs 2) '()]))
              (~optional (group
                          #:outer_check
                          (outer-tag::block
                           outer-check-g
                           ...))
                         #:defaults ([outer-tag #'#f]
                                     [(outer-check-g 1) '()]))
              (~optional (group
                          #:recur_binds
                          (_::block
                           (group loop-id:identifier _::equal loop-rhs ...)
                           ...))
                         #:defaults ([(loop-id 1) '()]
                                     [(loop-rhs 2) '()]))
              (~optional (group
                          #:head_guard
                          (head-tag::block
                           head-guard-g
                           ...))
                         #:defaults ([head-tag #'#f]
                                     [(head-guard-g 1) '()]))
              (~optional (group
                          #:inner_binds
                          (_::block
                           (group inner-ids::identifiers _::equal inner-rhs ...)
                           ...))
                         #:defaults ([(inner-ids 1) '()]
                                     [(inner-rhs 2) '()]))
              (~optional (group
                          #:pre_guard
                          (pre-tag::block
                           pre-guard-g
                           ...))
                         #:defaults ([pre-tag #'#f]
                                     [(pre-guard-g 1) '()]))
              (~optional (group
                          #:post_guard
                          (post-tag::block
                           post-guard-g
                           ...))
                         #:defaults ([post-tag #'#f]
                                     [(post-guard-g 1) '()]))
              (~optional (group #:recur_args
                                (_::block
                                 (group (_::parens
                                         recur-g
                                         ...))))
                         #:defaults ([(recur-g 1) '()])))
             #'[(lhs ...)
                (:do-in
                 ([(outer-ids.id ...) (rhombus-expression (group outer-rhs ...))] ...)
                 (rhombus-body-at* outer-tag outer-check-g ...)
                 ([loop-id (rhombus-expression (group loop-rhs ...))] ...)
                 (rhombus-body-at* head-tag head-guard-g ...)
                 ([(inner-ids.id ...) (rhombus-expression (group inner-rhs ...))] ...)
                 (rhombus-body-at* pre-tag pre-guard-g ...)
                 (rhombus-body-at* post-tag post-guard-g ...)
                 ((rhombus-expression recur-g) ...))]])]
         [else #f])])))

(define-syntax rhombus-body-at*
  (syntax-rules ()
    [(_ #f) #t]
    [(_ tag body ...) (rhombus-body-at tag body ...)]))
