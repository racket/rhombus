#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

(provide rhombus-declaration-sequence

         ;; wrap `rhombus-forward` around a sequence of declarations
         ;; to make any bindings among the  declarations visible only
         ;; after the declarations
         rhombus-forward)

(define-syntax (rhombus-declaration-sequence stx)
  (syntax-parse stx
    [(_ . tail)
     #'(sequence base-ctx add-ctx remove-ctx . tail)]))
    
(define-syntax (sequence stx)
  (syntax-parse stx
    [(_ base-ctx add-ctx remove-ctx) #'(begin)]
    [(_ base-ctx add-ctx remove-ctx form . forms)
     (define exp-form (local-expand #'form
                                    (syntax-local-context)
                                    (list #'rhombus-forward
                                          #'define-values
                                          #'define-syntaxes
                                          ;; etc.
                                          #'begin)
                                    #f))
     (syntax-parse exp-form
       #:literals (begin define-values rhombus-forward)
       [(rhombus-forward . sub-forms)
        (define introducer (make-syntax-introducer #t))
        #`(begin
            (sequence base-ctx #,(introducer #'add-ctx) base-ctx . sub-forms)
            (sequence base-ctx add-ctx #,(introducer #'remove-ctx) . #,(introducer #'forms)))]
       [(begin form ...)
        #`(sequence base-ctx add-ctx remove-ctx form ... . forms)]
       [((~and def (~or define-values define-syntaxes)) (id ...) rhs)
        #:with (new-id ...) ((make-syntax-delta-introducer #'remove-ctx #'base-ctx)
                             ((make-syntax-delta-introducer #'add-ctx #'base-ctx)
                              #'(id ...)
                              'add)
                             'remove)
        #`(begin
            #,(syntax/loc exp-form
                (def (new-id ...) rhs))
            (sequence base-ctx add-ctx remove-ctx . forms))]
       [_ #`(begin
              #,exp-form
              (sequence base-ctx add-ctx remove-ctx . forms))])]))

(define-syntax (rhombus-forward stx)
  (raise-syntax-error #f
                      "should not get expanded"
                      stx))
