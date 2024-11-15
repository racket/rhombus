#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/syntax-local
                     "introducer.rkt"
                     "macro-result.rkt"
                     (for-syntax racket/base))
         "enforest.rkt"
         "expression.rkt"
         "static-info.rkt"
         "index-result-key.rkt"
         "indirect-static-info-key.rkt"
         "parse.rkt"
         "treelist.rkt")

(provide define-repetition-syntax)
(begin-for-syntax
  (provide  (property-out repetition-prefix-operator)
            (property-out repetition-infix-operator)
            (struct-out repetition-prefix+infix-operator)

            repetition-transformer

            make-expression+repetition

            repetition-as-list
            repetition-as-nested-lists
            repetition-as-list/unchecked
            render-repetition

            flatten-repetition
            consume-repetition
            add-repetition-disappeared

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
    (pattern (rep-expr   ; for error reporting
              (~and for-clausess  ; a list of `for` binding-clause sets, outer to inner
                    ;; the length of this list determines the repetition depth
                    (([(iter-id ...) iter-rhs]
                      ...) ; inner layer clauses are in parallel
                     ...)) ; outer layer clauses are nested
              body       ; body to go inside `for`, produces one value
              element-static-infos ; describes the result of `body`
              used-depth:exact-nonnegative-integer))) ; depth of `rep-expr` already consumed, only for error reporting

  (define (make-repetition-info rep-expr for-clausess body element-static-infos used-depth)
    #`(#,rep-expr #,for-clausess #,body #,element-static-infos #,used-depth))

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
                          null
                          id
                          #`((#%indirect-static-info #,id))
                          0))

  (define (identifier-repetition-use/maybe id)
    (when (syntax-local-value* id (lambda (v)
                                    (and (or (expression-prefix-operator-ref v)
                                             (expression-infix-operator-ref v))
                                         (not (expression-repeatable-prefix-operator? v)))))
      (raise-syntax-error #f
                          "expression form does not support use as a repetition"
                          id))
    (make-repetition-info id
                          null
                          #`(rhombus-expression (group #,id))
                          #`((#%indirect-static-info #,id))
                          0))

  ;; Form in a repetition context:
  (define-rhombus-enforest
    #:syntax-class :repetition
    #:prefix-more-syntax-class :prefix-op+repetition-use+tail
    #:infix-more-syntax-class :infix-op+repetition-use+tail
    #:desc "repetition"
    #:operator-desc "repetition operator"
    #:parsed-tag #:rhombus/repet
    #:in-space in-repetition-space
    #:prefix-operator-ref repetition-prefix-operator-ref
    #:infix-operator-ref repetition-infix-operator-ref
    #:check-result check-repetition-result
    #:make-identifier-form identifier-repetition-use/maybe)

  (define (make-expression+repetition for-clausess for-body element-get-static-infos
                                      #:expr-handler [expr-handler (lambda (stx fail) (fail))]
                                      #:repet-handler [repet-handler (lambda (stx next) (next))])
    (values
     (expression-transformer
      (lambda (stx)
        (expr-handler stx (lambda ()
                            (syntax-parse stx
                              [(self . _)
                               (raise-syntax-error #f
                                                   "cannot use expression binding as a repetition"
                                                   #'self)])))))
     (repetition-transformer
      (lambda (stx)
        (repet-handler stx (lambda ()
                             (syntax-parse stx
                               [(id . tail)
                                (values (make-repetition-info #'id
                                                              for-clausess
                                                              for-body
                                                              (element-get-static-infos)
                                                              0)
                                        #'tail)])))))))

  (define (repetition-transformer proc)
    (repetition-prefix-operator '((default . stronger)) 'macro proc))

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
     (render-repetition/direct rep-parsed depth 'checked #'for/list)]))

(define-for-syntax (repetition-as-nested-lists rep-parsed depth for-form)
  (cond
    [(depth . > . 1)
     (repetition-as-nested-lists (consume-repetition rep-parsed for-form #'()) (sub1 depth) for-form)]
    [else
     (render-repetition/direct rep-parsed depth 'checked for-form)]))

(define-for-syntax (repetition-as-list/unchecked rep-parsed depth)
  (render-repetition/direct rep-parsed depth 'unchecked #'for/list))

(define-for-syntax (render-repetition for-form rep-parsed
                                      #:depth [depth 1])
  (render-repetition/direct rep-parsed depth 'checked for-form))

(define-for-syntax (render-repetition/direct rep-parsed depth mode for-form)
  (syntax-parse rep-parsed
    [rep-info::repetition-info
     (define want-depth (length (syntax->list #'rep-info.for-clausess)))
     (unless (or (eq? mode 'unchecked)
                 (= depth want-depth))
       (raise-wrong-depth #'rep-info.rep-expr
                          #'rep-info.used-depth
                          want-depth
                          depth))
     (define infos (if (identifier? #'rep-info.element-static-infos)
                       (extract-static-infos #'rep-info.element-static-infos)
                       #'rep-info.element-static-infos))
     (define (add-disappeared stx)
       (add-repetition-disappeared stx #'rep-info.rep-expr))
     (cond
       [(= depth 0)
        (wrap-static-info* (add-disappeared #'rep-info.body) infos)]
       [else
        (define seq-expr
          (add-disappeared
           (build-for for-form
                      (insert-clause-separators (syntax->list #'rep-info.for-clausess))
                      #'rep-info.body)))
        (if (null? (syntax-e infos))
            seq-expr
            (wrap-static-info seq-expr #'#%index-result infos))])]))

(define-for-syntax (flatten-repetition rep-parsed count
                                       #:pack-element [pack-element values]
                                       #:unpack-element [unpack-element values])
  (cond
    [(= 0 count) rep-parsed]
    [else
     (syntax-parse rep-parsed
       [rep-info::repetition-info
        (define for-clausess (syntax->list #'rep-info.for-clausess))
        (when (count . >= . (length for-clausess))
          (raise-wrong-depth #'rep-info.rep-expr
                             #'rep-info.used-depth
                             (length for-clausess)
                             (add1 count)
                             #:at-least? #t))
        (define keep-count (- (length for-clausess) (add1 count)))
        (make-repetition-info #'rep-info.rep-expr
                              (let loop ([keep-count keep-count] [for-clausess for-clausess])
                                (cond
                                  [(zero? keep-count)
                                   #`(([(elem) (in-list (for/list #,(insert-clause-separators for-clausess)
                                                          #,(pack-element #'rep-info.body)))]))]
                                  [else
                                   (cons (car for-clausess)
                                         (loop (sub1 keep-count) (cdr for-clausess)))]))
                              (unpack-element #'elem)
                              #'rep-info.element-static-infos
                              (+ (syntax-e #'rep-info.used-depth) count))])]))

(define-for-syntax (consume-repetition rep-parsed for-form static-infos)
  (syntax-parse rep-parsed
    [rep-info::repetition-info
     (define for-clausess (syntax->list #'rep-info.for-clausess))
     (when (null? for-clausess) (error "bad repetition nesting (internal error)"))
     (define keep-count (- (length for-clausess) 1))
     (make-repetition-info #'rep-info.rep-expr
                           (let loop ([keep-count keep-count] [for-clausess for-clausess])
                             (cond
                               [(zero? keep-count)
                                null]
                               [else
                                (cons (car for-clausess)
                                      (loop (sub1 keep-count) (cdr for-clausess)))]))
                           (build-for for-form
                                      (insert-clause-separators (list-tail for-clausess keep-count))
                                      #'rep-info.body)
                           #`((#%index-result rep-info.element-static-infos)
                              . #,static-infos)
                           (+ (syntax-e #'rep-info.used-depth) 1))]))

;; Optimize `for/list` over `in-list`, etc. We do this while
;; constructing the form, instead of using a `for/list` variant
;; that recognizes clauses, so that other shortcuts can apply,
;; especially for syntax objects. Note that we cannot easily optimize
;; maps and sets this way, since the predicate associated with the
;; map or set might be different than the constructed one.
(define-for-syntax (build-for for-form clauses body)
  (syntax-parse clauses
    [([(id) (in-form e)])
     #:when (and (or (and (free-identifier=? for-form #'for/list)
                          (free-identifier=? #'in-form #'in-list))
                     (and (free-identifier=? for-form #'for/treelist)
                          (free-identifier=? #'in-form #'in-treelist)))
                 (identifier? body)
                 (free-identifier=? body #'id))
     #'e]
    [else #`(#,for-form #,clauses #,body)]))

(define-for-syntax (raise-wrong-depth expr used-depth-stx want-depth actual-depth
                                      #:at-least? [at-least? #f])
  (raise-syntax-error #f
                      "used with wrong repetition depth"
                      expr
                      #f
                      null
                      (format "\n  expected: ~a\n  actual: ~a~a"
                              (+ want-depth (syntax-e used-depth-stx))
                              (if at-least? "at least " "")
                              (+ actual-depth (syntax-e used-depth-stx)))))

(define-syntax (define-repetition-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-repetition-space #'id)
         rhs)]))

(define-for-syntax (insert-clause-separators clauses-stxs)
  (let loop ([clauses-stxs clauses-stxs])
    (cond
      [(null? clauses-stxs) null]
      [(null? (cdr clauses-stxs)) (syntax->list (car clauses-stxs))]
      [else (append (syntax->list (car clauses-stxs))
                    (list '#:when #t)
                    (loop (cdr clauses-stxs)))])))

(define-for-syntax (add-repetition-disappeared stx rep-expr)
  (if (identifier? rep-expr)
      (syntax-property stx
                       'disappeared-use
                       (syntax-local-introduce rep-expr))
      stx))
