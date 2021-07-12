#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     "op.rkt"))

(provide (for-syntax parse-top))

(begin-for-syntax
  (struct rhombus-form (expansion))
  (struct rhombus-definition rhombus-form ())
  (struct rhombus-expression rhombus-form ())

  (struct rhombus-transformer (proc))
  ;; returns an expression and a tail of unused syntax:
  (struct rhombus-expression-transformer rhombus-transformer ())
  ;; returns a list of definitions+expressions and a list of expressions:
  (struct rhombus-definition-transformer rhombus-transformer ())
  ;; returns a list of declarations+definitions+expression:
  (struct rhombus-declaration-transformer rhombus-transformer ())

  (define-syntax-class operator
    (pattern ((~datum op) name) #:attr opname #'name))

  (define-syntax-class declaration
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-declaration-transformer? v)
             #:attr expandeds (apply-declaration-transformer v (cons #'head #'tail))))

  (define-syntax-class definition
    (pattern ((~datum group) head:identifier . tail)
             #:do [(define v (syntax-local-value #'head (lambda () #f)))]
             #:when (rhombus-definition-transformer? v)
             #:do [(define-values (defns-and-exprs exprs)
                     (apply-definition-transformer v (cons #'head #'tail)))]
             #:attr expandeds defns-and-exprs
             #:attr exprs exprs))

  (define-syntax-class expression
    (pattern ((~datum group) . tail) #:attr expanded (enforest #'tail)))

  (define (enforest #:init-form [init-form #f] stxes)
    (let loop ([init-form init-form] [stxes stxes]
               [combine (lambda (form) form)] [current-op #f] [stack '()])
      (define (keep form/s context tail alone-name adjacent-name #:multi? [multi? #f])
        (cond
          [(not init-form)
           (loop (if alone-name
                     (alone-combine context alone-name form/s
                                    #:multi? multi?)
                     form/s)
                 tail
                 combine current-op stack)]
          [else
           (loop (adjacent-combine context adjacent-name init-form form/s
                                   #:multi? multi?)
                 tail
                 combine current-op stack)]))
      (syntax-parse stxes
        [()
         (cond
           [(not init-form) (raise-syntax-error #f "empty expression")]
           [(null? stack) (combine init-form)]
           [else (loop (combine init-form) stxes (caar stack) (cdar stack) (cdr stack))])]
        [(head:operator . tail)
         (define v (syntax-local-value #'head.opname (lambda () #f)))
         (cond
           [(and (rhombus-binary-operator? v)
                 (or init-form
                     (not (rhombus-unary-operator? v))))
            (unless init-form
              (raise-syntax-error #f "binary operator without preceding argument" #'head))
            (define rel-prec (if (not current-op)
                                 'higher
                                 (relative-precedence v current-op #'head)))
            (cond
              [(or (not current-op) (eq? rel-prec 'higher))
               (loop #f #'tail
                     (lambda (form) (apply-binary-operator v init-form form #'head))
                     v
                     (cons (cons combine current-op) stack))]
              [(eq? rel-prec 'lower)
               (unless (pair? stack) (error 'internal "empty enforest stack"))
               (loop (combine init-form) stxes
                     (caar stack) (cdar stack) (cdr stack))]
              [else
               (cond
                 [(eq? rel-prec 'same)
                  (raise-syntax-error #f
                                      "non-associative operator needs explicit parenthesization"
                                      #'head.opname)]
                 [else
                  (raise-syntax-error #f
                                      (format
                                       (string-append 
                                        "combination of operators without declared relative precedence"
                                        " needs explicit parenthesization\n"
                                        "  other operator: ~a")
                                       (syntax-e (rhombus-operator-name current-op)))
                                      #'head.opname)])])]
           [(rhombus-unary-operator? v)
            (loop init-form #'tail
                  (lambda (form) (apply-unary-operator v form #'head))
                  v
                  (cons (cons combine current-op) stack))]
           [else
            (raise-syntax-error #f "unbound operator" #'head.name)])]
        [(head:identifier . tail)
         (define v (syntax-local-value #'head (lambda () #f)))
         (cond
           [(rhombus-expression-transformer? v)
            (define-values (form tail) (apply-expression-transformer v stxes))
            (keep form #'head tail #f juxtipose-name)]
           [(rhombus-transformer? v)
            (raise-syntax-error #f "illegal use" #'head)]
           [else
            ;; identifier is a variable
            (keep #'head #'head #'tail #f juxtipose-name)])]
        [(((~and tag (~datum parens)) . _) . tail)
         (keep (parse-expression-sequence (stx-car stxes)) #'tag #'tail tuple-name call-name #:multi? #t)]
        [(((~and tag (~datum braces)) . _) . tail)
         (keep (parse-expression-sequence (stx-car stxes)) #'tag #'tail array-name ref-name #:multi? #t)]
        [(((~and tag (~datum block)) . _) . tail)
         (keep (parse-expression-block (stx-car stxes)) #'tag #'tail #f juxtipose-name)]
        [(((~and tag (~datum alts)) . inside) . tail)
         (raise-syntax-error 'alternation
                             "misplaced alternative"
                             (stx-car stxes)
                             #'tag)]
        [(datum . tail)
         (keep (syntax/loc #'datum (#%datum . datum)) #'datum #'tail #f juxtipose-name)])))

  ;; returns: 'higher, 'lower, 'same (no associativity), #f (not related)
  (define (relative-precedence op other-op head)
    (define (find op ids)
      (for/or ([id (in-list ids)])
        (free-identifier=? (rhombus-operator-name op) id)))
    (define op-lo? (find other-op (rhombus-operator-less-than-names op)))
    (define op-same? (find other-op (rhombus-operator-same-as-names op)))
    (define op-hi? (find other-op (rhombus-operator-greater-than-names op)))
    (define ot-lo? (find op (rhombus-operator-less-than-names other-op)))
    (define ot-same? (find op (rhombus-operator-same-as-names other-op)))
    (define ot-hi? (find op (rhombus-operator-greater-than-names other-op)))
    (define (raise-inconsistent how)
      (raise-syntax-error #f
                           (format
                            (string-append "inconsistent operator ~a declared\n"
                                           "  one operator: ~a\n"
                                           "  other operator: ~a")
                            how
                            (syntax-e (rhombus-operator-name op))
                            (syntax-e (rhombus-operator-name other-op)))
                           head))
    (cond
      [(or (and op-lo? (or ot-lo? ot-same?))
           (and op-same? (or ot-lo? ot-hi?))
           (and op-hi? (or ot-hi? ot-same?)))
       (raise-inconsistent "precedence")]
      [(or op-lo? ot-hi?) 'lower]
      [(or op-hi? ot-lo?) 'higher]
      [(or op-same? ot-same?
           (free-identifier=? (rhombus-operator-name op)
                              (rhombus-operator-name other-op)))
       (define op-a (rhombus-binary-operator-assoc op))
       (when (rhombus-binary-operator? other-op)
         (unless (eq? op-a (rhombus-binary-operator-assoc other-op))
           (raise-inconsistent "associativity")))
       (case op-a
         [(left) 'lower]
         [(right) 'higher]
         [else 'same])]
      [else #f]))

  (define (alone-combine adj-context alone-name form/s #:multi? multi?)
    (define v (syntax-local-value (datum->syntax adj-context alone-name) (lambda () #f)))
    (unless (if multi?
                (rhombus-multi-unary-operator? v)
                (rhombus-unary-operator? v))
      (raise-syntax-error #f
                          (format "misplaced expression;\n implicit `~a` is not a ~aunary operator in this context"
                                  alone-name
                                  (if multi? "multi-" ""))
                          adj-context))
    (apply-unary-operator v form/s adj-context #:multi? multi?))

  (define (adjacent-combine adj-context adjacent-name form1 form2/s #:multi? multi?)
    (define v (syntax-local-value (datum->syntax adj-context adjacent-name) (lambda () #f)))
    (unless (if multi?
                (rhombus-multi-binary-operator? v)
                (rhombus-binary-operator? v))
      (raise-syntax-error #f
                          (format "misplaced expression;\n implicit `~a` is not a ~abinary operator in this context"
                                  adjacent-name
                                  (if multi? "multi-" ""))
                          adj-context))
    (apply-binary-operator v form1 form2/s adj-context #:multi? multi?))

  (define (apply-unary-operator v form/s stx #:multi? [multi? #f])
    (define form ((if multi?
                      (rhombus-multi-unary-operator-proc v)
                      (rhombus-unary-operator-proc v))
                  form/s
                  stx))
    (unless (syntax? form) (raise-result-error 'rhombus-unary-operator "syntax?" form))
    form)

  (define (apply-binary-operator v form1 form2/s stx #:multi? [multi? #f])
    (define form ((if multi?
                      (rhombus-multi-binary-operator-proc v)
                      (rhombus-binary-operator-proc v))
                  form1
                  form2/s
                  stx))
    (unless (syntax? form) (raise-result-error 'rhombus-binary-operator "syntax?" form))
    form)
  
  (define (apply-expression-transformer t stx)
    (call-with-values
     (lambda () ((rhombus-transformer-proc t) stx))
     (case-lambda
       [(form tail)
        (unless (syntax? form) (raise-result-error 'rhombus-tranform "syntax?" form))
        (unless (stx-list? tail) (raise-result-error 'rhombus-tranform "stx-list?" tail))
        (values form tail)])))

  (define (apply-definition-transformer t stx)
    (call-with-values
     (lambda () ((rhombus-transformer-proc t) stx))
     (case-lambda
       [(forms exprs)
        (unless (stx-list? forms) (raise-result-error 'rhombus-tranform-definition "stx-list?" forms))
        (unless (stx-list? exprs) (raise-result-error 'rhombus-tranform-definition "stx-list?" exprs))
        (values forms exprs)])))

  (define (apply-declaration-transformer t stx)
    (call-with-values
     (lambda () ((rhombus-transformer-proc t) stx))
     (case-lambda
       [(forms)
        (unless (stx-list? forms) (raise-result-error 'rhombus-tranform-declaration "stx-list?" forms))
        forms])))

  (define (parse-expression-block orig-stx)
    (when (stx-null? orig-stx)
      (raise-syntax-error #f "found an empty block" orig-stx))
    #`(let ()
        #,@(let loop ([stx (stx-cdr orig-stx)])
            (syntax-parse stx 
              [() '()]
              [(e:definition . tail)
               (when (and (stx-null? #'tail)
                          (stx-null? #'e.exprs))
                 (raise-syntax-error #f "block does not end with an expression" stx))
               #`((begin . e.expandeds)
                  (begin . e.exprs)
                  . #,(loop #'tail))]
              [(e:expression . tail)
               #`(e.expanded
                  . #,(loop #'tail))]))))

  (define (parse-expression-sequence orig-stx)
    (let loop ([stx (stx-cdr orig-stx)])
      (syntax-parse stx 
        [() '()]
        [(e:expression . tail)
         (cons #'e.expanded (loop #'tail))])))

  (define (parse-top stx)
    (syntax-parse stx
      [()
       (raise-syntax-error #f "found an empty top form" stx)]
      [e:declaration #'(begin . e.expandeds)]
      [e:definition #'(begin (begin . e.expandeds) . e.exprs)]
      [e:expression #'e.expanded])))
