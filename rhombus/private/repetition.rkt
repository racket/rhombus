#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "operator-parse.rkt"
                     enforest
                     enforest/property
                     enforest/syntax-local
                     enforest/operator
                     enforest/transformer
                     enforest/property
                     enforest/name-parse
                     enforest/proc-name
                     "introducer.rkt"
                     "macro-result.rkt"
                     (for-syntax racket/base))
         "enforest.rkt"
         "expression.rkt"
         "binding.rkt"
         "static-info.rkt"
         "ref-result-key.rkt"
         "indirect-static-info-key.rkt"
         "parse.rkt")

(provide define-repetition-syntax)
(begin-for-syntax
  (provide  (property-out repetition-prefix-operator)
            (property-out repetition-infix-operator)
            (struct-out repetition-prefix+infix-operator)

            repetition-transformer

            make-expression+repetition

            repetition-as-list
            repetition-as-deeper-repetition
            flatten-repetition

            :repetition
            :repetition-info
            :prefix-op+repetition-use+tail
            :infix-op+repetition-use+tail

            in-repetition-space
            repet-quote
            

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
    #`(#,rep-expr #,name #,seq-expr #,bind-depth #,use-depth #,element-static-infos #,immediate?))

  (define (check-repetition-result form proc)
    (syntax-parse (if (syntax? form) form #'#f)
      [_::repetition-info form]
      [_ (raise-bad-macro-result (proc-name proc) "repetition" form)]))

  (property repetition-prefix-operator prefix-operator)
  (property repetition-infix-operator infix-operator)
  
  (struct repetition-prefix+infix-operator (prefix infix)
    #:property prop:repetition-prefix-operator (lambda (self) (repetition-prefix+infix-operator-prefix self))
    #:property prop:repetition-infix-operator (lambda (self) (repetition-prefix+infix-operator-infix self)))

  (define in-repetition-space (make-interned-syntax-introducer/add 'rhombus/repet))
  (define-syntax (repet-quote stx)
    (syntax-case stx ()
      [(_ id) #`(quote-syntax #,((make-interned-syntax-introducer 'rhombus/repet) #'id))]))

  (define (identifier-repetition-use id)
    (make-repetition-info id
                          id
                          id
                          0
                          0
                          #`((#%indirect-static-info #,id))
                          #t))

  (define (identifier-repetition-use/maybe id)
    (make-repetition-info id
                          id
                          #`(rhombus-expression (group #,id))
                          0
                          0
                          #`((#%indirect-static-info #,id))
                          #t))

  ;; Form in a repetition context:
  (define-rhombus-enforest
    #:syntax-class :repetition
    #:prefix-more-syntax-class :prefix-op+repetition-use+tail
    #:infix-more-syntax-class :infix-op+repetition-use+tail
    #:desc "repetition"
    #:operator-desc "repetition operator"
    #:in-space in-repetition-space
    #:prefix-operator-ref repetition-prefix-operator-ref
    #:infix-operator-ref repetition-infix-operator-ref
    #:check-result check-repetition-result
    #:make-identifier-form identifier-repetition-use/maybe)

  (define (make-expression+repetition name seq-expr element-static-infos
                                      #:depth [depth 1]
                                      #:expr-handler [expr-handler (lambda (stx fail) (fail))]
                                      #:repet-handler [repet-handler (lambda (stx next) (next))])
    (values
     (expression-transformer
      (lambda (stx)
        (expr-handler stx (lambda ()
                            (syntax-parse stx
                              [(self . _)
                               (raise-syntax-error #f
                                                   "cannot use repetition binding as an expression"
                                                   #'self)])))))
     (repetition-transformer
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
                                        #'tail)])))))))

  (define (repetition-transformer proc)
    (repetition-prefix-operator (quote-syntax ignored) '((default . stronger)) 'macro proc))

  (define (repetition-static-info-lookup element-static-infos key)
    (if (identifier? element-static-infos)
        (syntax-local-static-info element-static-infos key)
        (static-info-lookup element-static-infos key))))

(define-for-syntax repetition-as-list
  (case-lambda
    [(ellipses stx depth)
     (repetition-as-list ellipses stx depth 0)]
    [(ellipses stx depth extra-ellipses)
     (syntax-parse stx
       [rep::repetition
        (repetition-as-list (flatten-repetition #'rep.parsed extra-ellipses) depth)]
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
        (define infos (if (identifier? #'rep-info.element-static-infos)
                          (datum->syntax #f (extract-static-infos #'rep-info.element-static-infos))
                          #'rep-info.element-static-infos))
        (if (= depth 0)
            (wrap-static-info* #'rep-info.seq-expr infos)
            (wrap-static-info #'rep-info.seq-expr #'#%ref-result infos))])]))

(define-for-syntax (repetition-as-deeper-repetition rep-parsed static-infos)
  (syntax-parse rep-parsed
    [rep-info::repetition-info
     (make-repetition-info #'rep-info.rep-expr
                           #'rep-info.name
                           #'rep-info.seq-expr
                           #'rep-info.bind-depth
                           (+ 1 (syntax-e #'rep-info.use-depth))
                           #`((#%ref-result rep-info.element-static-infos)
                              . #,static-infos)
                           #'rep-info.immediate?)]))

(define-for-syntax (flatten-repetition rep-parsed count)
  (cond
    [(= 0 count) rep-parsed]
    [else
     (syntax-parse rep-parsed
       [rep-info::repetition-info
        (make-repetition-info #'rep-info.rep-expr
                              #'rep-info.name
                              #`(flatten rep-info.seq-expr #,count)
                              #'rep-info.bind-depth
                              (+ count (syntax-e #'rep-info.use-depth))
                              #'rep-info.element-static-infos
                              #f)])]))

(define (flatten lists count)
  (if (zero? count)
      lists
      (flatten (apply append lists) (sub1 count))))

(define-syntax (define-repetition-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-repetition-space #'id)
         rhs)]))
