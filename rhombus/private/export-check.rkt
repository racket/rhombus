#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "introducer.rkt"))

(provide (for-syntax register-field-check
                     register-provide-check))

;; Resgisters a check that an identifie is defined; this check
;; needs to be deferred until all definitions in the module have
;; been discovered
(define-for-syntax (register-field-check stx)
  (when (syntax-transforming-module-expression?)
    (syntax-local-lift-module-end-declaration #`(check-export-bound . #,stx))))

(define-for-syntax (check-identifier base-ctx forward-ctx id
                                     [spaces (cons #f (syntax-local-module-interned-scope-symbols))])
  (unless (for/or ([space (in-list spaces)])
            (identifier-binding ((space->introducer space) id)))
    (if (let ([intro (make-syntax-delta-introducer forward-ctx base-ctx)])
          (for/or ([space (in-list spaces)])
            (identifier-binding ((space->introducer space) (intro id 'add)))))
        (raise-syntax-error #f "exported identifier defined later, but not in scope for export" id)
        (raise-syntax-error #f "exported identifier not defined" id))))

(define-syntax (check-export-bound stx)
  (syntax-parse stx
    [(_ base-ctx forward-ctx . fields)
     (for ([field (in-list (syntax->list #'fields))])
       (define (check id rule)
         (syntax-parse rule
           [() (check-identifier #'base-ctx #'forward-ctx id)]
           [(#:space _ . rule)
            (check id #'rule)]
           [(#:only space ...)
            (check-identifier #'base-ctx #'forward-ctx id (syntax->datum #'(space ...)))]
           [(#:except space ...)
            (define ht (for/hasheq ([space (in-list (cons #f (syntax-local-module-interned-scope-symbols)))])
                         (values space #t)))
            (check-identifier #'base-ctx #'forward-ctx id
                              (hash-keys (for/fold ([ht ht]) ([space (in-list (syntax->datum #'(space ...)))])
                                           (hash-remove ht #f))))]))
       (syntax-parse field
         [id:identifier
          (check #'id #'())]
         [(ext-id int-id . rule)
          (check #'int-id #'rule)]
         [_ (error "oops" field)]))
     #'(void)]))

(define-for-syntax (register-provide-check stx)
  (syntax-local-lift-module-end-declaration #`(check-export-provide . #,stx)))

;; The job of this checker is to povide a better error message in the
;; case that an identifer is defined via `let`. So, we don't have to check
;; antyhing invconvenient, such as a restriction to a particular space
(define-syntax (check-export-provide stx)
  (syntax-parse stx
    [(_ base-ctx forward-ctx . exs)
     (for ([ex (in-list (syntax->list #'exs))])
       (let loop ([ex ex])
         (syntax-parse ex
           #:datum-literals (combine-out all-spaces-out all-from-out for-meta for-label
                                         only-spaces-out except-spaces-out all-spaces-defined-out
                                         except-out)
           [(combine-out ex ...)
            (for ([ex (in-list (syntax->list #'(ex ...)))])
              (loop ex ))]
           [(all-spaces-out o ...)
            (for ([o (in-list (syntax->list #'(o ...)))])
              (define id
                (syntax-parse o
                  [(int-id ext-id) #'int-id]
                  [_:identifier o]))
              (check-identifier #'base-ctx #'forward-ctx id))]
           [_
            (void)])))])
  #'(void))
