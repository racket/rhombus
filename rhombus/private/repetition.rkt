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
         "ref-result-key.rkt"
         "parse.rkt")

(provide define-repetition-syntax)
(begin-for-syntax
  (provide  (property-out repetition-prefix-operator)
            (property-out repetition-infix-operator)

            repetition-transformer
            make-expression+repetition-prefix-operator
            make-expression+repetition-infix-operator
            make-expression+binding+repetition-prefix-operator
            make-expression+binding+repetition-infix-operator
            expression+repetition-prefix+infix-operator
            make-expression+repetition-transformer

            make-repetition

            repetition-as-list

            :repetition
            :repetition-info

            in-repetition-space

            identifier-repetition-use

            make-repetition-info

            repetition-static-info-lookup))

(begin-for-syntax
  (define-syntax-class :repetition-info
    #:datum-literals (parens group)
    (pattern (rep-expr
              name
              seq-expr
              bind-depth:exact-nonnegative-integer
              use-depth:exact-nonnegative-integer
              element-static-infos
              immediate?)))

  (define (make-repetition-info rep-expr name seq-expr bind-depth use-depth element-static-infos immediate?)
    ;; `element-static-infos` can be an identifier, which means both that static
    ;; information can be looked up on demand
    #`(#,rep-expr #,name #,seq-expr #,bind-depth #,use-depth #,element-static-infos #,immediate?))

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

  (struct expression+repetition-infix-operator (exp-op rep-op)
    #:property prop:expression-infix-operator (lambda (self) (expression+repetition-infix-operator-exp-op self))
    #:property prop:repetition-infix-operator (lambda (self) (expression+repetition-infix-operator-rep-op self)))
  (define (make-expression+repetition-infix-operator name prec protocol exp rep assc)
    (expression+repetition-infix-operator
     (expression-infix-operator name prec protocol exp assc)
     (repetition-infix-operator name prec protocol rep assc)))

  (define (make-expression+repetition-transformer name exp rep)
    (make-expression+repetition-prefix-operator name '((default . stronger)) 'macro exp rep))

  (struct expression+repetition-prefix+infix-operator (prefix infix)
    #:property prop:expression-prefix-operator (lambda (self)
                                                 (expression+repetition-prefix-operator-exp-op
                                                  (expression+repetition-prefix+infix-operator-prefix self)))
    #:property prop:expression-infix-operator (lambda (self)
                                                (expression+repetition-infix-operator-exp-op
                                                 (expression+repetition-prefix+infix-operator-infix self)))
    #:property prop:repetition-prefix-operator (lambda (self)
                                                 (expression+repetition-prefix-operator-rep-op
                                                  (expression+repetition-prefix+infix-operator-prefix self)))
    #:property prop:repetition-infix-operator (lambda (self)
                                                (expression+repetition-infix-operator-rep-op
                                                 (expression+repetition-prefix+infix-operator-infix self))))

  (struct expression+binding+repetition-prefix-operator (exp-op bind-op rep-op)
    #:property prop:expression-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-exp-op self))
    #:property prop:binding-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-bind-op self))
    #:property prop:repetition-prefix-operator (lambda (self) (expression+binding+repetition-prefix-operator-rep-op self)))
  (define (make-expression+binding+repetition-prefix-operator name prec protocol exp bind rep)
    (expression+binding+repetition-prefix-operator
     (expression-prefix-operator name prec protocol exp)
     (binding-prefix-operator name prec protocol bind)
     (repetition-prefix-operator name prec protocol rep)))

  (struct expression+binding+repetition-infix-operator (exp-op bind-op rep-op)
    #:property prop:expression-infix-operator (lambda (self) (expression+binding+repetition-infix-operator-exp-op self))
    #:property prop:binding-infix-operator (lambda (self) (expression+binding+repetition-infix-operator-bind-op self))
    #:property prop:repetition-infix-operator (lambda (self) (expression+binding+repetition-infix-operator-rep-op self)))
  (define (make-expression+binding+repetition-infix-operator name prec protocol exp bind rep assc)
    (expression+binding+repetition-infix-operator
     (expression-infix-operator name prec protocol exp assc)
     (binding-infix-operator name prec protocol bind assc)
     (repetition-infix-operator name prec protocol rep assc)))

  (define in-repetition-space (make-interned-syntax-introducer/add 'rhombus/repetition))

  (define (identifier-repetition-use id)
    (make-repetition-info id
                          id
                          id
                          0
                          0
                          id
                          #t))

  (define (identifier-repetition-use/maybe id)
    (make-repetition-info id
                          id
                          #`(rhombus-expression (group #,id))
                          0
                          0
                          id
                          #t))

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
    #:make-identifier-form identifier-repetition-use/maybe)

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
                               (values (make-repetition-info stx
                                                             name
                                                             seq-expr
                                                             depth
                                                             #'0
                                                             element-static-infos
                                                             #f)
                                       #'tail)]))))))

  (define (repetition-transformer name proc)
    (repetition-prefix-operator name '((default . stronger)) 'macro proc))

  (define (repetition-static-info-lookup element-static-infos key)
    (if (identifier? element-static-infos)
        (syntax-local-static-info element-static-infos key)
        (static-info-lookup element-static-infos key))))

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
                              "used with wrong repetition depth"
                              #'rep-info.rep-expr
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
