#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         "for-clause.rkt"
         "parens.rkt"
         "parse.rkt"
         (submod "values.rkt" for-parse)
         (submod "membership-testable.rkt" in-operator)
         (submod "equal.rkt" for-parse))

(provide (for-space rhombus/for_clause
                    each
                    keep_when
                    skip_when
                    break_when
                    final_when
                    keep_let))

(begin-for-syntax
  (define (check-multiple-ins stx)
    (syntax-parse stx
      [(_ _ ... in::in (~seq _ ... more::in) ...+ _ ...)
       (raise-syntax-error #f
                           (string-append "multiple immediate membership operators not allowed in this group;"
                                          "\n use parentheses to disambiguate")
                           stx
                           #'in
                           (syntax->list #'(more ...)))]
      [(_ _ ... _::in _ ...)
       (void)]))

  ;; Like `:var-decl`, but `in` is recognized in place of `=`
  (define-splicing-syntax-class (:each-decl stx)
    #:datum-literals (group)
    #:attributes ([bind-g 1] blk)
    (pattern (~seq (~optional _::values-id-bind) (_::parens bind-g ...) (~and blk (_::block . _))))
    (pattern (~and (~seq t ...)
                   (~seq (~optional _::values-id-bind) (_::parens bind-g ...) _::in expr ...+))
             #:do [(check-multiple-ins #'(t ...))]
             #:with blk #`(block (#,group-tag expr ...)))
    (pattern (~and (~seq t ...)
                   (~seq bind ...+ _::in expr ...+))
             #:do [(check-multiple-ins #'(t ...))]
             #:with (bind-g ...) #`((#,group-tag bind ...))
             #:with blk #`(block (#,group-tag expr ...)))
    (pattern (~seq bind ...+ (~and blk (_::block . _)))
             #:with (bind-g ...) #`((#,group-tag bind ...)))))

(define-for-clause-syntax each
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~var d (:each-decl stx)))
        #`(#:each ((d.bind-g ...)) (d.blk))]
       [(form-id (tag::block (group (~var d (:each-decl stx))) ...))
        #`(#:each ((d.bind-g ...) ...) (d.blk ...))]
       [_
        (raise-syntax-error #f
                            "needs a binding or a block of bindings, each followed by a block"
                            (respan stx))]))))

(define-for-syntax (parse-when stx kw)
  (syntax-parse stx
    [(form-id (tag::block g ...))
     #`(#,kw (rhombus-body-at tag g ...))]
    [(form-id expr ...+)
     #`(#,kw (rhombus-expression (#,group-tag expr ...)))]
    [(form-id)
     (raise-syntax-error #f
                         "missing expression"
                         (respan #'stx))]))

(define-for-clause-syntax keep_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:when))))

(define-for-clause-syntax skip_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:unless))))

(define-for-clause-syntax break_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:break))))

(define-for-clause-syntax final_when
  (for-clause-transformer
   (lambda (stx)
     (parse-when stx '#:final))))

(define-for-clause-syntax keep_let
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~optional _::values-id-bind) (_::parens g ...) (~and rhs (_::block . _)))
        #`(#:let (g ...) rhs)]
       [(form-id (~optional _::values-id-bind) (_::parens g ...) _::equal rhs ...+)
        (check-multiple-equals stx)
        #`(#:let (g ...) (#,group-tag rhs ...))]
       [(form-id bind ...+ _::equal rhs ...+)
        (check-multiple-equals stx)
        #`(#:let ((#,group-tag bind ...)) (#,group-tag rhs ...))]
       [(form-id bind ...+ (~and rhs (_::block . _)))
        #`(#:let ((#,group-tag bind ...)) rhs)]))))
