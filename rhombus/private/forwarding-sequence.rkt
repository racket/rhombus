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
    [(_ #:need-end-expr orig . tail)
     #'(sequence #:need-end-expr orig base-ctx add-ctx remove-ctx . tail)]
    [(_ . tail)
     #'(sequence #f #f base-ctx add-ctx remove-ctx . tail)]))

(define-syntax (sequence stx)
  (let loop ([stx stx] [accum null])
    (syntax-parse stx
      [(_ mode orig base-ctx add-ctx remove-ctx)
       (when (and (eq? (syntax-e #'mode) '#:need-end-expr)
                  (syntax-e #'orig))
         (raise-syntax-error #f "block does not end with an expression" #'orig))
       #`(begin #,@(reverse accum))]
      [(_ mode orig base-ctx add-ctx remove-ctx (~and form ((~literal quote) v)) . forms)
       (loop #'(_ mode orig base-ctx add-ctx remove-ctx . forms)
             (cons #'form accum))]
      [(_ mode orig base-ctx add-ctx remove-ctx form . forms)
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
         #:literals (begin define-values define-syntaxes rhombus-forward)
         [(rhombus-forward . sub-forms)
          (define introducer (make-syntax-introducer #t))
          #`(begin
              #,@(reverse accum)
              (sequence #f #f base-ctx #,(introducer #'add-ctx) base-ctx . sub-forms)
              (sequence mode orig base-ctx add-ctx #,(introducer #'remove-ctx) . #,(introducer #'forms)))]
         [(begin form ...)
          (define seq #`(sequence mode orig base-ctx add-ctx remove-ctx form ... . forms))
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
              (sequence #:need-end-expr orig base-ctx add-ctx remove-ctx . forms))]
         [_ #`(begin
                #,@(reverse accum)
                #,exp-form
                (sequence #:saw-non-defn #f base-ctx add-ctx remove-ctx . forms))])])))

(define-syntax (rhombus-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))
