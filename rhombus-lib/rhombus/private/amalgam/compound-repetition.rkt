#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "tag.rkt")
         "expression.rkt"
         "repetition.rkt"
         "parse.rkt"
         "static-info.rkt"
         "order.rkt"
         "ends-parse.rkt")

(provide (for-syntax make-expression&repetition-prefix-operator
                     make-expression&repetition-infix-operator

                     repetition-depth

                     build-compound-repetition))

(begin-for-syntax
  ;; `kind` can be
  ;;  - 'prefix -- actual prefix operator
  ;;  - 'nofix  -- "nofix" operator that consumes nothing
  ;;  - 'mixfix -- both prefix and "nofix", depending on the tail
  (define (make-expression&repetition-prefix-operator order prec kind exp
                                                      #:element-statinfo? [element-statinfo? #f])
    (define (prefix-exp form self-stx)
      (exp form self-stx))
    (define (prefix-rep form self-stx)
      (build-compound-repetition self-stx
                                 (list form)
                                 (lambda (form)
                                   (define expr (exp form self-stx))
                                   (values (discard-static-infos expr)
                                           (extract-static-infos expr)))
                                 #:element-statinfo? element-statinfo?))
    (define (nofix-exp stx)
      (syntax-parse stx
        [(self . tail)
         (define expr (exp #'self))
         (values expr #'tail)]))
    (define (nofix-rep stx)
      (define-values (expr tail) (nofix-exp stx))
      (define repet (make-repetition-info stx
                                          '()
                                          (discard-static-infos expr)
                                          (extract-static-infos expr)
                                          0))
      (values repet tail))
    (define (mixfix-exp stx)
      (syntax-parse stx
        [(self::name . more)
         #:when (do-ends-parse? 'prefix #'self.name #'more
                                in-expression-space expression-relative-precedence expression-infix-operator-ref)
         (nofix-exp stx)]
        [(self . more)
         #:with (~var rhs (:prefix-op+expression+tail #'self)) #`(#,group-tag . more)
         (define expr (prefix-exp #'rhs.parsed #'self))
         (values expr #'rhs.tail)]))
    (define (mixfix-rep stx)
      (syntax-parse stx
        [(self::name . more)
         #:when (do-ends-parse? 'prefix #'self.name #'more
                                in-repetition-space repetition-relative-precedence repetition-infix-operator-ref)
         (nofix-rep stx)]
        [(self . more)
         #:with (~var rhs (:prefix-op+repetition-use+tail #'self)) #`(#,group-tag . more)
         (define expr (prefix-rep #'rhs.parsed #'self))
         (values expr #'rhs.tail)]))
    (define-values (final-exp final-rep protocol)
      (case kind
        [(prefix)
         (values prefix-exp prefix-rep 'automatic)]
        [(nofix)
         (values nofix-exp nofix-rep 'macro)]
        [(mixfix)
         (values mixfix-exp mixfix-rep 'macro)]
        [else
         (error "unrecognized kind")]))
    (values
     (expression-prefix-operator order prec protocol final-exp)
     (repetition-prefix-operator order (add-repet-space prec) protocol final-rep)))

  ;; `kind` can be
  ;;  - 'infix   -- actual infix operator
  ;;  - 'postfix -- postfix operator that consumes nothing
  ;;  - 'mixfix  -- both infix and postfix, depending on the tail
  (define (make-expression&repetition-infix-operator order prec kind exp assc
                                                     #:element-statinfo? [element-statinfo? #f])
    (define infix-exp
      (case-lambda
        [(form1 form2 self-stx)
         (exp form1 form2 self-stx)]
        [(form1 form2 self-stx mode)
         (exp form1 form2 self-stx mode)]))
    (define (do-rep-infix form1 form2 self-stx exp)
      (build-compound-repetition self-stx
                                 (list form1 form2)
                                 (lambda (form1 form2)
                                   (define expr (exp form1 form2 self-stx))
                                   (values (discard-static-infos expr)
                                           (extract-static-infos expr)))
                                 #:element-statinfo? element-statinfo?))
    (define infix-rep
      (case-lambda
        [(form1 form2 self-stx)
         (do-rep-infix form1 form2 self-stx exp)]
        [(form1 form2 self-stx mode)
         (do-rep-infix form1 form2 self-stx (lambda (form1 form2 self-stx)
                                              (exp form1 form2 self-stx mode)))]))
    (define (postfix-exp form stx)
      (syntax-parse stx
        [(self . tail)
         (define expr (exp form #'self))
         (values expr #'tail)]))
    (define (postfix-rep form stx)
      (syntax-parse stx
        [(self . tail)
         (define repet (build-compound-repetition #'self
                                                  (list form)
                                                  (lambda (form)
                                                    (define expr (exp form #'self))
                                                    (values (discard-static-infos expr)
                                                            (extract-static-infos expr)))
                                                  #:element-statinfo? element-statinfo?))
         (values repet #'tail)]))
    (define (mixfix-exp form stx)
      (syntax-parse stx
        [(self::name . more)
         #:when (do-ends-parse? 'infix #'self.name #'more
                                in-expression-space expression-relative-precedence expression-infix-operator-ref)
         (postfix-exp form stx)]
        [(self . more)
         #:with (~var rhs (:infix-op+expression+tail #'self)) #`(#,group-tag . more)
         (define expr (infix-exp form #'rhs.parsed #'self))
         (values expr #'rhs.tail)]))
    (define (mixfix-rep form stx)
      (syntax-parse stx
        [(self::name . more)
         #:when (do-ends-parse? 'infix #'self.name #'more
                                in-repetition-space repetition-relative-precedence repetition-infix-operator-ref)
         (postfix-rep form stx)]
        [(self . more)
         #:with (~var rhs (:infix-op+repetition-use+tail #'self)) #`(#,group-tag . more)
         (define expr (infix-rep form #'rhs.parsed #'self))
         (values expr #'rhs.tail)]))
    (define-values (final-exp final-rep protocol)
      (case kind
        [(infix)
         (values infix-exp infix-rep 'automatic)]
        [(postfix)
         (values postfix-exp postfix-rep 'macro)]
        [(mixfix)
         (values mixfix-exp mixfix-rep 'macro)]
        [else
         (error "unrecognized kind")]))
    (values
     (expression-infix-operator order prec protocol final-exp assc)
     (repetition-infix-operator order (add-repet-space prec) protocol final-rep assc)))

  (define (add-repet-space prec)
    (lambda ()
      (for/list ([p (in-list (if (procedure? prec)
                                 (prec)
                                 prec))])
        (if (and (identifier? (car p))
                 (not (bound-identifier=? (car p) (in-order-space (car p)))))
            (cons (in-repetition-space (car p)) (cdr p))
            p))))

  (define (repetition-depth form)
    (syntax-parse form
      [rep::repetition-info
       (length (syntax->list #'rep.for-clausess))])))

(define-for-syntax (build-compound-repetition at-stx forms build-one
                                              #:sequence-for-form [sequence-for-form #'for/list]
                                              #:is-sequence? [is-sequence? (lambda (form) #f)]
                                              #:element-statinfo? [element-statinfo? #f]
                                              #:extract [extract (lambda (form) form)])
  (define depth
    (for/fold ([depth 0]) ([form (in-list forms)])
      (max depth (- (repetition-depth (extract form))
                    (if (is-sequence? form) 1 0)))))
  (define-values (for-clausesss bodys)
    (for/lists (for-clausesss bodys)
               ([form (in-list forms)])
      (syntax-parse (extract form)
        [rep::repetition-info
         (define for-clausess (syntax->list #'rep.for-clausess))
         (define (add-disappeared stx)
           (add-repetition-disappeared stx #'rep.rep-expr))
         (cond
           [(is-sequence? form)
            (define rev-for-clausess (reverse for-clausess))
            (values (reverse (cdr rev-for-clausess))
                    (add-disappeared
                     #`(#,sequence-for-form #,(car rev-for-clausess)
                        rep.body)))]
           [else
            (define body (add-disappeared #'rep.body))
            (values for-clausess
                    (if element-statinfo?
                        (wrap-static-info*
                         body
                         #'rep.element-static-infos)
                        body))])])))
  (define-values (body static-infos)
    (apply build-one bodys))
  (make-repetition-info at-stx
                        (let loop ([depth depth]
                                   [for-clausesss for-clausesss])
                          (cond
                            [(zero? depth)
                             null]
                            [else
                             (cons
                              (apply
                               append
                               (for/list ([for-clausess (in-list for-clausesss)]
                                          #:when (= (length for-clausess) depth))
                                 (syntax->list (car for-clausess))))
                              (loop (sub1 depth)
                                    (for/list ([for-clausess (in-list for-clausesss)])
                                      (if (= (length for-clausess) depth)
                                          (cdr for-clausess)
                                          for-clausess))))]))
                        body
                        static-infos
                        0))
