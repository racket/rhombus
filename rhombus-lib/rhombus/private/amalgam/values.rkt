#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "srcloc.rkt")
         "provide.rkt"
         "binding.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "reducer.rkt"
         "static-info.rkt"
         "function-arity-key.rkt"
         "values-key.rkt"
         (submod "define-arity.rkt" for-info)
         "parens.rkt"
         "op-literal.rkt"
         "var-decl.rkt"
         "simple-call.rkt"
         "parse.rkt"
         "compound-repetition.rkt")

(provide (for-spaces (rhombus/bind
                      rhombus/annot
                      rhombus/reducer
                      rhombus/statinfo)
                     values)
         (for-spaces (rhombus/reducer)
                     (rename-out [values fold]))
         (for-spaces (#f
                      rhombus/repet)
                     (rename-out [rhombus-values values]))
         (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [call-with-values call_with_values])))

(module+ for-parse
  (provide (for-syntax :values-id-annot
                       :values-id-bind)))

(define-syntax rhombus-values
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head (_::parens g ...) . tail)
        #:when (simple-call? stx)
        (syntax-parse #'(g ...)
          [(e::expression ...)
           (define es (syntax->list #'(e.parsed ...)))
           (cond
             [(= 1 (length es))
              (values (wrap-static-info*
                       #`(values #,(discard-static-infos (car es)))
                       (extract-static-infos (car es)))
                      #'tail)]
             [else
              (values (wrap-static-info
                       #`(values #,@(map discard-static-infos es))
                       #'#%values
                       (datum->syntax #f (map extract-static-infos es)))
                      #'tail)])])]
       [(head . tail)
        (values (relocate-id #'head #'values)
                #'tail)]))))

(define-repetition-syntax rhombus-values
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head (_::parens g ...) . tail)
        #:when (simple-call? stx #:repetition? #t)
        (syntax-parse #'(g ...)
          [(e::repetition ...)
           (define es (syntax->list #'(e.parsed ...)))
           (define sis
             (for/list ([e (in-list es)])
               (syntax-parse e
                 [rep::repetition-info
                  #'rep.element-static-infos])))
           (values
            (build-compound-repetition
             #'head
             es
             (lambda new-content
               (values #`(values #,@new-content)
                       (if (= 1 (length sis))
                           (car sis)
                           #`((#%values #,sis))))))
            #'tail)])]
       [(head . tail)
        (values (identifier-repetition-use (relocate-id #'head #'values))
                #'tail)]))))

(define-binding-syntax values
  (binding-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as a pattern (except as a non-nested"
                                           " pattern by forms that specifically recognize it)")
                            #'head)]))))

(define-annotation-syntax values
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(head . _)
        (raise-syntax-error #f
                            (string-append "not allowed as an annotation (except as a non-nested"
                                           " annotation by forms that specifically recognize it)")
                            #'head)]))))

(begin-for-syntax
  (define-syntax-rule (define-values-id-class class in-space quote)
    (define-syntax-class class
      #:attributes (name)
      #:description "the literal `values`"
      #:opaque
      (pattern ::name
               #:when (free-identifier=? (in-space #'name)
                                         (quote values)))))
  (define-values-id-class :values-id-annot in-annotation-space annot-quote)
  (define-values-id-class :values-id-bind in-binding-space bind-quote))

(begin-for-syntax
  (define-splicing-syntax-class :accum
    #:description "accumulator with optional annotation"
    #:attributes (id e check make-check static-infos)
    (pattern d::var-decl
      #:with ((~or* (~and _::_-bind
                          (~parse id (car (generate-temporaries '(wildcard)))))
                    id:identifier)
              (~optional c::inline-annotation))
      #'(d.bind ...)
      #:with e #'d.default
      #:do [(define-values (check make-check)
              (if (cond
                    [(attribute c.converter) => syntax-e]
                    [else #f])
                  (values (car (generate-temporaries '(check)))
                          #'(lambda (who)
                              (lambda (val)
                                (c.converter
                                 val who
                                 (lambda (val who)
                                   (raise-annotation-failure who val 'c.annotation-str))))))
                  (values #f #f)))]
      #:attr check check
      #:attr make-check make-check
      #:with static-infos #'(~? c.static-infos ()))))

(define-reducer-syntax values
  (reducer-transformer
   (lambda (stx)
     (syntax-parse (respan stx)
       #:datum-literals (group)
       [(form-id (_::parens (group accum::accum) ...) . tail)
        (values
         (reducer/no-break #'build-values-check-bind
                           #'([accum.id (~? (let ([accum.id accum.e])
                                              (accum.check accum.id)
                                              accum.id)
                                            accum.e)]
                              ...)
                           #:pre-clause #'build-values-static-info
                           #'build-values-check
                           #'((#%values (accum.static-infos
                                         ...)))
                           #'([accum.id accum.static-infos
                                        (~? [accum.check (accum.make-check 'form-id)])]
                              ...))
         #'tail)]))))

(define-syntax (build-values-check-bind stx)
  (syntax-parse stx
    [(_ ([_ _ (~optional (~and check-bind [_ _]))] ...) e)
     #'(let ((~? check-bind) ...)
         e)]))

(define-syntax (build-values-static-info stx)
  (syntax-parse stx
    [(_ ([id si (~optional [_ _])] ...))
     #'(begin
         (define-static-info-syntax/maybe id . si)
         ...)]))

(define-syntax (build-values-check stx)
  (syntax-parse stx
    [(_ ([id _ (~optional [check _])] ...) e)
     #'(let-values ([(id ...) e])
         (~? (check id))
         ...
         (values id ...))]))

(define-static-info-syntax values
  (#%function-arity -1)
  . #,(indirect-get-function-static-infos))

(define-static-info-syntax call-with-values
  (#%function-arity 4)
  . #,(indirect-get-function-static-infos))
