#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     version/utils
                     "parse.rkt")
         "definition.rkt"
         "parse.rkt"
         "parens.rkt"
         (submod "equal.rkt" for-parse)
         (only-in "equal.rkt"
                  [= rhombus=])
         (only-in "def+let.rkt" def)
         (only-in "block.rkt" block))

(provide (for-space rhombus/defn
                    sequence_macro)
         (for-syntax make_sequence_constructor))

(define-defn-syntax sequence_macro
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ id (tag::block rhs-g))
         (list
          #'(define-sequence-syntax id
              (lambda () #'values)
              (rhombus-expression rhs-g)))]))))

(begin-for-syntax
  (define-syntax-class :binding+rhs
    #:attributes ([bind 1] [rhs 1])
    #:datum-literals (group)
    (pattern (~and g
                   (group bind ...+ _::equal rhs ...+))
             #:do [(check-multiple-equals #'g)])
    (pattern (group bind ...+ (b-tag::block body ...))
             #:with (rhs ...) #'(block (b-tag body ...)))))

(define-for-syntax (make_sequence_constructor proc)
  (lambda (stx)
    (define approx-for-old-racket? (version<? (version) "8.10.0.3"))
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
                           outer::binding+rhs
                           ...)))
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
                           inner::binding+rhs
                           ...)))
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
             #`[(lhs ...)
                (:do-in
                 ()
                 (begin
                   #,(if (attribute outer)
                         #'(rhombus-body-sequence
                            (group def outer.bind ... rhombus= outer.rhs ...)
                            ...)
                         #'(begin))
                   (rhombus-body-at* outer-tag outer-check-g ...))
                 ([loop-id (rhombus-expression (group loop-rhs ...))] ...)
                 (rhombus-body-at* head-tag head-guard-g ...)
                 #,(if (and (attribute inner)
                            approx-for-old-racket?)
                       #`([(inner.bind ...) (rhombus-expression (group inner.rhs ...))]
                          ...)
                       #'())
                 #,@(if (and (attribute inner)
                             (not approx-for-old-racket?))
                        (list
                         #'(begin
                             (rhombus-body-sequence
                              (group def inner.bind ... rhombus= inner.rhs ...)
                              ...)))
                        null)
                 (rhombus-body-at* pre-tag pre-guard-g ...)
                 (rhombus-body-at* post-tag post-guard-g ...)
                 ((rhombus-expression recur-g) ...))]])]
         [else #f])])))

(define-syntax rhombus-body-at*
  (syntax-rules ()
    [(_ #f) #t]
    [(_ tag body ...) (rhombus-body-at tag body ...)]))
