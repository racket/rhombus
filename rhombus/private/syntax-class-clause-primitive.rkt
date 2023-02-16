#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "syntax-class-clause.rkt"
         "parens.rkt"
         "parse.rkt")

(provide (for-space rhombus/syntax_class_clause
                    pattern
                    description
                    kind
                    error_mode
                    fields
                    root_swap))

(module+ for-class
  (provide (for-syntax extract-clauses)))

(define-for-syntax (extract-clauses stx clauses)
  (define options
    (for/fold ([options #hasheq()]) ([clause (in-list clauses)])
      (define (check what)
        (syntax-parse clause
          [(kw (~and orig-stx (_ id . _)) . _)
           (when (hash-ref options (syntax-e #'kw) #f)
             (raise-syntax-error #f
                                 (string-append "found second " what "clause, but only one is allowed")
                                 stx
                                 #'orig-stx))]))
      (syntax-parse clause
        [(#:pattern _ alts)
         (check "pattern")
         (hash-set options '#:pattern (syntax->list #'alts))]
        [(#:description _ e)
         (check "description")
         (hash-set options '#:description #'e)]
        [(#:fields _ ht)
         (check "fields")
         (hash-set options '#:fields (syntax-e #'ht))]
        [(#:root_swap _ to-root root-to)
         (check "root_swap")
         (hash-set options '#:root_swap (cons #'to-root #'root-to))]
        [(#:kind _ kw)
         (check "kind")
         (hash-set options '#:kind (syntax-e #'kw))]
        [(#:error-mode _ kw)
         (check "error mode")
         (hash-set options '#:error-mode (syntax-e #'kw))])))
  (define alts (hash-ref options '#:pattern #f))
  (unless alts
    (raise-syntax-error #f
                        "missing a pattern clause"
                        stx))
  (values alts
          (hash-ref options '#:kind #f)
          (hash-ref options '#:description #f)
          (hash-ref options '#:fields #f)
          (hash-ref options '#:root_swap #f)
          (eq? (hash-ref options '#:error-mode #f) '#:opaque)))

(define-syntax-class-clause-syntax pattern
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::alts b ...))
        #`(#:pattern #,stx (b ...))]
       [(_ (~and pat (_::quotes . _)))
        #`(#:pattern #,stx ((block (group pat))))]
       [(_ (~and pat (_::quotes . _)) (~and b (_::block . _)))
        #`(#:pattern #,stx ((block (group pat b))))]))))

(define-syntax-class-clause-syntax description
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (tag::block e ...+))
        #`(#:description #,stx (rhombus-body-at tag e ...))]
       [(_ e ...)
        #`(#:description #,stx (rhombus-expression e ...))]))))

(define-syntax-class-clause-syntax fields
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (tag::block (group id:identifier ...) ...))
        (define ht (for/fold ([ht #hasheq()]) ([id (in-list (syntax->list #'(id ... ...)))])
                     (define sym (syntax-e id))
                     (if (hash-ref ht sym #f)
                         (raise-syntax-error #f
                                             "duplicate field name"
                                             stx
                                             id)
                         (hash-set ht sym id))))
        #`(#:fields #,stx #,ht)]))))

(define-syntax-class-clause-syntax root_swap
  (syntax-class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (tag::block (group to-root:identifier root-to:identifier)))
        #`(#:root_swap #,stx to-root root-to)]))))

(define-for-syntax (make-kw-clause tag-kw valid what)
  (syntax-class-clause-transformer
   (lambda (stx)
     (define (parse-keyword kw)
       (if (memq (syntax-e kw) valid)
           kw
           (raise-syntax-error #f
                               (string-append "not a recognized " what)
                               stx
                               kw)))
     (syntax-parse stx
       #:datum-literals (group)
       [(_ kw:keyword) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]
       [(_ (_::block (group kw:keyword))) #`(#,tag-kw #,stx #,(parse-keyword #'kw))]))))

(define-syntax-class-clause-syntax kind (make-kw-clause '#:kind '(#:term #:sequence #:group #:multi #:block) "kind"))
(define-syntax-class-clause-syntax error_mode (make-kw-clause '#:error-mode '(#:opaque #:transparent) "error mode"))
