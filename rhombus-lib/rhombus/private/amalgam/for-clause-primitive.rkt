#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         "for-clause.rkt"
         "parens.rkt"
         "parse.rkt"
         (only-in "implicit.rkt" #%body) ; for `block` in `:each-decl`
         (submod "membership-testable.rkt" in-operator))

(provide (for-space rhombus/for_clause
                    each
                    keep_when
                    skip_when
                    break_when
                    final_when))

(begin-for-syntax
  ;; Like `:var-decl`, but we don't allow `=` here
  (define-splicing-syntax-class (:each-decl stx)
    #:datum-literals (group)
    #:attributes ([bind 1] blk)
    (pattern (~seq bind ...+ _::in expr ...+)
             #:do [(syntax-parse #'(bind ... expr ...)
                     [(_ ... i::in _ ...)
                      (raise-syntax-error #f
                                          (string-append "multiple immediate membership operators not allowed in this group;"
                                                         "\n use parentheses to disambiguate")
                                          stx
                                          #'i)]
                     [_ (void)])]
             #:attr blk #'(block (group expr ...)))
    (pattern (~seq bind ...+ (~and blk (_::block . _))))))

(define-for-clause-syntax each
  (for-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~var d (:each-decl stx)))
        #`(#:each d.bind ... d.blk)]
       [(form-id (tag::block (group (~var d (:each-decl stx))) ...))
        #`(#:each (tag (group d.bind ... d.blk)
                       ...))]
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
