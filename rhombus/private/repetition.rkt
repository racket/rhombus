#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "operator-parse.rkt"
                     enforest
                     enforest/property
                     enforest/syntax-local
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/name-parse
                     enforest/proc-name
                     "name-path-op.rkt"
                     "introducer.rkt")
         "name-root-ref.rkt"
         "expression.rkt"
         "binding.rkt"
         "static-info.rkt"
         "ref-result-key.rkt")

(provide
 (for-syntax make-repetition
             make-expression+binding+repetition-prefix-operator
             repetition-transformer

             repetition-as-list

             :repetition
             :repetition-info

             in-repetition-space

             make-repetition-info)
 define-repetition-syntax)

(begin-for-syntax
  (define-syntax-class :repetition-info
    #:datum-literals (parens group)
    (pattern (name
              seq-expr
              bind-depth:exact-nonnegative-integer
              use-depth:exact-nonnegative-integer
              element-static-infos)))

  (define (make-repetition-info name seq-expr bind-depth use-depth element-static-infos)
    #`(#,name #,seq-expr #,bind-depth #,use-depth #,element-static-infos))

  (define (check-repetition-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::repetition-info form]
      [_ (raise-result-error (proc-name proc) "repetition-info?" form)]))

  (property repetition-prefix-operator prefix-operator)
  (property repetition-infix-operator infix-operator)

  (struct expression+repetition-prefix-operator (exp-op rep-op)
    #:property prop:expression-prefix-operator (lambda (self) (expression+repetition-prefix-operator-exp-op self))
    #:property prop:repetition-prefix-operator (lambda (self) (expression+repetition-prefix-operator-rep-op self)))
  (define (make-expression+repetition-prefix-operator name prec protocol exp rep)
    (expression+repetition-prefix-operator
     (expression-prefix-operator name prec protocol exp)
     (repetition-prefix-operator name prec protocol rep)))

  (struct expression+binding+repetition-prefix-operator (exp-op bind-op rep-op)
    #:property prop:expression-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-exp-op self))
    #:property prop:binding-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-bind-op self))
    #:property prop:repetition-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-rep-op self)))
  (define (make-expression+binding+repetition-prefix-operator name prec protocol exp bind rep)
    (expression+binding+repetition-prefix-operator
     (expression-prefix-operator name prec protocol exp)
     (binding-prefix-operator name prec protocol bind)
     (repetition-prefix-operator name prec protocol rep)))

  (define in-repetition-space (make-interned-syntax-introducer/add 'rhombus/repetition))

  (define (identifier-repetition-use id)
    (make-repetition-info id
                          id
                          0
                          0
                          #'()))

  ;; Form in a repetition context:
  (define-enforest
    #:syntax-class :repetition
    #:prefix-more-syntax-class :prefix-op+repetition-use+tail
    #:infix-more-syntax-class :infix-op+repetition-use+tail
    #:desc "repetition"
    #:operator-desc "repetition operator"
    #:in-space in-repetition-space
    #:name-path-op name-path-op
    #:name-root-ref name-root-ref
    #:name-root-ref-root name-root-ref-root
    #:prefix-operator-ref repetition-prefix-operator-ref
    #:infix-operator-ref repetition-infix-operator-ref
    #:check-result check-repetition-result
    #:make-identifier-form identifier-repetition-use)

  (define (make-repetition name seq-expr element-static-infos
                           #:depth [depth 1]
                           #:expr-handler [expr-handler (lambda (stx fail) (fail))]
                           #:repet-handler [repet-handler (lambda (stx next) (next))])
    (make-expression+repetition-prefix-operator
     name '((default . stronger)) 'macro
     (lambda (stx)
       (expr-handler stx (lambda ()
                           (syntax-parse stx
                             [(self . _)
                              (raise-syntax-error #f
                                                  "cannot use repetition binding as an expression"
                                                  #'self)]))))
     (lambda (stx)
       (repet-handler stx (lambda ()
                            (syntax-parse stx
                              [(id . tail)
                               (values (make-repetition-info name
                                                             seq-expr
                                                             depth
                                                             #'0
                                                             element-static-infos)
                                       #'tail)]))))))

  (define (repetition-transformer name proc)
    (repetition-prefix-operator name '((default . stronger)) 'macro proc)))

(define-for-syntax repetition-as-list
  (case-lambda
    [(ellipses stx depth)
     (syntax-parse stx
       [rep::repetition
        (repetition-as-list #'rep.parsed depth)]
       [_
        (raise-syntax-error (syntax-e ellipses)
                            "not preceded by a repetition"
                            stx)])]
    [(rep-parsed depth)
     (syntax-parse rep-parsed
       [rep-info::repetition-info
        (define want-depth (syntax-e #'rep-info.bind-depth))
        (define use-depth (+ depth (syntax-e #'rep-info.use-depth)))
        (unless (= use-depth want-depth)
          (raise-syntax-error #f
                              "used with wrong ellipsis depth"
                              #'rep-info.name
                              #f
                              null
                              (format "\n  expected: ~a\n  actual: ~a"
                                      want-depth
                                      use-depth)))
        (wrap-static-info #'rep-info.seq-expr
                          #'#%ref-result
                          #'rep-info.element-static-infos)])]))

(define-syntax (define-repetition-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-repetition-space #'id)
         rhs)]))
