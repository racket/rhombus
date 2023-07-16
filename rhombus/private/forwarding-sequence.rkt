#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "syntax-parameter.rkt")

;; The `rhombus-forwarding-sequence` form handles definitions that are
;; only visible to later terms (as created with Rhombus `let`, say,
;; and exposed to here by a `rhombus-forward` wrapper). It also takes
;; care of syntax parameters and making nested `import` work through
;; lifting. The `rhombus-nested-forwarding-sequence` form extends that
;; to gather `export` information for a nested context.

(provide rhombus-forwarding-sequence
         rhombus-nested-forwarding-sequence
         rhombus-mixed-forwarding-sequence
         rhombus-mixed-nested-forwarding-sequence

         ;; wrap `rhombus-forward` around a sequence of declarations
         ;; to make any bindings among the  declarations visible only
         ;; after the declarations
         rhombus-forward)

(define-syntax (rhombus-forwarding-sequence stx)
  (syntax-parse stx
    [(_ ctx mode orig . tail)
     #`(sequence ctx mode orig base-ctx add-ctx remove-ctx #hasheq() . tail)]))

(define-syntax (rhombus-nested-forwarding-sequence stx)
  ;; Used for something like `namespace`
  (syntax-parse stx
    [(_ final . tail)
     #`(sequence [final] #f #f base-ctx add-ctx remove-ctx #hasheq() . tail)]))

(define-syntax (rhombus-mixed-forwarding-sequence stx)
  ;; Used for something like `class`, where non-expression, non-definition
  ;; forms are expanded to `(quote-syntax (stop-id . _))` and gathered to
  ;; be passed along to `final`
  (syntax-parse stx
    [(_ (final . data) stop-id . tail)
     #`(sequence [(final . data) #:stop-at stop-id] #f #f base-ctx add-ctx remove-ctx #hasheq() . tail)]))

(define-syntax (rhombus-mixed-nested-forwarding-sequence stx)
  ;; Actually used by `class`, which acts like `namespace`, too
  (syntax-parse stx
    [(_ (final . data) stop-id . tail)
     #`(sequence [(final . data) #:stop-at* stop-id ()] #f #f base-ctx add-ctx remove-ctx #hasheq() . tail)]))

(define-syntax (sequence stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ ctx mode orig base-ctx add-ctx remove-ctx stx-params)
       (when (and (eq? (syntax-e #'mode) '#:need-end-expr)
                  (syntax-e #'orig))
         (raise-syntax-error #f "block does not end with an expression" #'orig))
       (define forms #`(begin #,@(reverse accum)))
       (syntax-parse #'ctx
         [[(final ...) #:stop-at _ accum ...]
          #`(begin
              #,forms
              (final ... #,@(reverse (syntax->list #'(accum ...)))))]
         [[(final ...) #:stop-at* _ binds accum ...]
          #`(begin
              #,forms
              (final ... [#:ctx base-ctx remove-ctx] #,(reverse (syntax->list #'binds)) #,@(reverse (syntax->list #'(accum ...)))))]
         [[(final ...) bind ...]
          #`(begin
              #,forms
              (final ... [#:ctx base-ctx remove-ctx] #,@(reverse (syntax->list #'(bind ...)))))]
         [_ forms])]
      [(_ ctx mode orig base-ctx add-ctx remove-ctx stx-parsm (~and form ((~literal quote) v)) . forms)
       (loop #'(_ ctx mode orig base-ctx add-ctx remove-ctx stx-params . forms)
             (cons #'form accum))]
      [(_ ctx mode orig base-ctx add-ctx remove-ctx stx-params form . forms)
       (define exp-form (syntax-parse #'form
                          #:literals (module module*)
                          [(module . _) #'form]
                          [(module* . _) #'form]
                          [else
                           (with-continuation-mark
                            syntax-parameters-key #'stx-params
                            (local-expand #'form
                                          (syntax-local-context)
                                          (list #'rhombus-forward
                                                #'pop-forward
                                                #'define-values
                                                #'define-syntaxes
                                                #'define-syntax-parameter
                                                ;; etc.
                                                #'begin
                                                #'provide
                                                #'#%require
                                                #'#%declare
                                                #'begin-for-syntax)
                                          #f))]))
       (syntax-parse exp-form
         #:literals (begin define-values define-syntaxes rhombus-forward pop-forward #%require provide #%provide quote-syntax
                           define-syntax-parameter)
         [(rhombus-forward sub-form ...)
          (define introducer (make-syntax-introducer #t))
          #`(begin
              #,@(reverse accum)
              (sequence ctx #f #f base-ctx #,(introducer #'add-ctx) base-ctx stx-params
                        sub-form ...
                        (pop-forward mode orig base-ctx add-ctx #,(introducer #'remove-ctx)
                                     . #,(introducer #'forms))))]
         [(pop-forward mode orig base-ctx add-ctx remove-ctx . forms)
          #`(sequence ctx mode orig base-ctx add-ctx remove-ctx stx-params . forms)]
         [(define-syntax-parameter key rhs)
          (with-syntax ([stx-params (syntax-parameter-update #'key #'rhs #'stx-params)])
            #`(sequence ctx #:need-end-expr orig base-ctx add-ctx remove-ctx stx-params . forms))]
         [(begin form-in ...)
          #:with (form ...) (map (lambda (form)
                                   (shift-origin form exp-form))
                                 (syntax->list #'(form-in ...)))
          (define seq #`(sequence ctx mode orig base-ctx add-ctx remove-ctx stx-params form ... . forms))
          (if (null? accum)
              seq
              #`(begin #,@(reverse accum) #,seq))]
         [((~and def (~or define-values define-syntaxes)) (id ...) rhs)
          #:with (new-id ...) ((make-syntax-delta-introducer #'remove-ctx #'base-ctx)
                               ((make-syntax-delta-introducer #'add-ctx #'base-ctx)
                                #'(id ...)
                                'add)
                               'remove)
          #`(begin
              #,@(reverse accum)
              #,(datum->syntax exp-form
                               (syntax-e #`(def (new-id ...)
                                             #,(if (eq? (syntax-e #'def) 'define-syntaxes)
                                                   #`(with-continuation-mark
                                                      syntax-parameters-key (quote-syntax stx-params)
                                                      rhs)
                                                   #'(with-syntax-parameters stx-params
                                                       rhs))))
                               exp-form
                               exp-form)
              (sequence ctx #:need-end-expr orig base-ctx add-ctx remove-ctx stx-params . forms))]
         [(#%require req ...)
          (define intro (let ([sub (make-syntax-delta-introducer #'remove-ctx #'base-ctx)]
                              [add (make-syntax-delta-introducer #'add-ctx #'base-ctx)])
                          (lambda (stx)
                            (sub (add stx 'add) 'remove))))
          (define reqs
            (for/list ([req (in-list (cdr (syntax->list exp-form)))])
              (syntax-parse req
                #:datum-literals (portal)
                [((~and tag portal) id content) #`(tag #,(intro #'id) content)]
                [_ (intro req)])))
          (cond
            [(eq? (syntax-e #'ctx) '#:block)
             (for ([req (in-list reqs)])
               (syntax-local-lift-require (syntax-local-introduce req) #'use #f))
             #`(sequence ctx mode orig base-ctx add-ctx remove-ctx stx-params . forms)]
            [else
             #`(begin
                 #,(syntax-track-origin #`(#%require #,@reqs) exp-form #'none)
                 (sequence ctx mode orig base-ctx add-ctx remove-ctx stx-params . forms))])]
         [(provide prov ...)
          #:when (not (keyword? (syntax-e #'ctx)))
          (define rev-prov (reverse (syntax->list #'(prov ...))))
          (define new-ctx
            (syntax-parse #'ctx
              [(head #:stop-at* stop-id binds-tail . tail)
               #`(head #:stop-at* stop-id (#,@rev-prov . binds-tail) . tail)]
              [(head . tail)
               #`(head #,@rev-prov . tail)]))
          #`(sequence #,new-ctx mode orig base-ctx add-ctx remove-ctx stx-params . forms)]
         [(#%provide . _)
          (raise-syntax-error #f "shouldn't happen" exp-form)]
         [(quote-syntax (~and keep (id:identifier . _)) #:local)
          #:do [(define next
                  (syntax-parse #'ctx
                    [(head #:stop-at stop-id . tail)
                     (free-identifier=? #'id #'stop-id)
                     (syntax-track-origin
                      #`(begin
                          #,@(reverse accum)
                          (sequence (head #:stop-at stop-id [keep stx-params] . tail) #:saw-non-defn #f base-ctx add-ctx remove-ctx stx-params . forms))
                      exp-form
                      #'none)]
                    [(head #:stop-at* stop-id binds . tail)
                     (free-identifier=? #'id #'stop-id)
                     (syntax-track-origin
                      #`(begin
                          #,@(reverse accum)
                          (sequence (head #:stop-at* stop-id binds [keep stx-params] . tail) #:saw-non-defn #f base-ctx add-ctx remove-ctx stx-params . forms))
                      exp-form
                      #'none)]
                    [_ #f]))]
          #:when next
          next]
         [_ #`(begin
                #,@(reverse accum)
                #,(if (zero? (hash-count (syntax-e #'stx-params)))
                      exp-form
                      (syntax-parse exp-form
                        #:literals (#%declare begin-for-syntax module module*)
                        [((~or #%declare begin-for-syntax module module*) . _)
                         exp-form]
                        [_ #`(#%expression
                              (with-syntax-parameters stx-params #,exp-form))]))
                (sequence ctx #:saw-non-defn #f base-ctx add-ctx remove-ctx stx-params . forms))])])))

(define-syntax (rhombus-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))

(define-syntax (pop-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))
