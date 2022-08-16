#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide rhombus-forwarding-sequence

         ;; wrap `rhombus-forward` around a sequence of declarations
         ;; to make any bindings among the  declarations visible only
         ;; after the declarations
         rhombus-forward)

(define-syntax (rhombus-forwarding-sequence stx)
  (syntax-parse stx
    [(_ ctx mode orig . tail)
     ;; add a scope to the body that is not included on any lifted `require`s,
     ;; which ensures that the lifts are shadowed by local definitions
     (define intro (make-syntax-introducer #t))
     #`(sequence ctx mode orig base-ctx add-ctx remove-ctx #,(intro #'req-remove-ctx)
                 . #,(intro #'tail))]))

(define-syntax (sequence stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx)
       (when (and (eq? (syntax-e #'mode) '#:need-end-expr)
                  (syntax-e #'orig))
         (raise-syntax-error #f "block does not end with an expression" #'orig))
       #`(begin #,@(reverse accum))]
      [(_ ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx (~and form ((~literal quote) v)) . forms)
       (loop #'(_ ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx . forms)
             (cons #'form accum))]
      [(_ ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx form . forms)
       (define exp-form (local-expand #'form
                                      (syntax-local-context)
                                      (list #'rhombus-forward
                                            #'define-values
                                            #'define-syntaxes
                                            ;; etc.
                                            #'begin
                                            #'provide
                                            #'require
                                            #'#%require
                                            #'#%provide
                                            #'begin-for-syntax)
                                      #f))
       (syntax-parse exp-form
         #:literals (begin define-values define-syntaxes rhombus-forward #%require)
         [(rhombus-forward . sub-forms)
          (define introducer (make-syntax-introducer #t))
          #`(begin
              #,@(reverse accum)
              (sequence ctx #f #f base-ctx #,(introducer #'add-ctx) base-ctx base-ctx . sub-forms)
              (sequence ctx mode orig base-ctx add-ctx #,(introducer #'remove-ctx) #,(introducer #'req-remove-ctx)
                        . #,(introducer #'forms)))]
         [(begin form ...)
          (define seq #`(sequence ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx form ... . forms))
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
                               (syntax-e #'(def (new-id ...) rhs))
                               exp-form
                               exp-form)
              (sequence ctx #:need-end-expr orig base-ctx add-ctx remove-ctx req-remove-ctx . forms))]
         [(#%require req ...)
          (define intro (let ([sub (make-syntax-delta-introducer #'req-remove-ctx #'base-ctx)]
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
             #`(sequence ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx . forms)]
            [else
             #`(begin
                 (#%require #,@reqs)
                 (sequence ctx mode orig base-ctx add-ctx remove-ctx req-remove-ctx . forms))])]
         [_ #`(begin
                #,@(reverse accum)
                #,exp-form
                (sequence ctx #:saw-non-defn #f base-ctx add-ctx remove-ctx req-remove-ctx . forms))])])))

(define-syntax (rhombus-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))
