#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in enforest: enforest/name-root)
                     "srcloc.rkt"))

;; convert a hierachical layer implemented as portal syntax to a name-root

(provide (for-syntax name-root-ref))

(define-for-syntax (name-root-ref v)
  (define (make get)
    (enforest:name-root
     (lambda (stxes)
       (syntax-parse stxes
         #:datum-literals (op parens |.|)
         [(form-id (op |.|) field:identifier . tail)
          (values (relocate #'field (get #'form-id "identifier" #'field)) #'tail)]
         [(form-id (op |.|) (parens (group (~and target (op field))))  . tail)
          (values (relocate #'target #`(op #,(get #'form-id "operator" #'field))) #'tail)]
         [(form-id (op (~and dot |.|)) . tail)
          (raise-syntax-error #f
                              "expected an identifier or parentheses after dot"
                              #'dot)]
         [(form-id . tail)
          (raise-syntax-error #f
                              "expected a dot after name"
                              #'form-id)]))))
  (or
   (enforest:name-root-ref v)
   (and
    (portal-syntax? v)
    (syntax-parse (portal-syntax-content v)
      [([(~datum import) _] pre-ctx-s ctx-s)
       (define pre-ctx #'pre-ctx-s)
       (define ctx #'ctx-s)
       (make (lambda (who-stx what name)
               (define id (datum->syntax ctx
                                         (syntax-e name)
                                         name
                                         name))
               (define pre-id (datum->syntax pre-ctx (syntax-e name)))
               (unless (identifier-distinct-binding id pre-id)
                 (raise-syntax-error #f
                                     (format "no such imported ~a" what)
                                     name))
               id))]
      [((~datum map) [key val] ...)
       (define keys (syntax->list #'(key ...)))
       (define vals (syntax->list #'(val ...)))
       (make (lambda (who-stx what name)
               (or (for/or ([key (in-list keys)]
                            [val (in-list vals)])
                     (and (eq? (syntax-e key) (syntax-e name))
                          val))
                   (raise-syntax-error #f
                                       (format "~a not provided by ~a"
                                               what
                                               (syntax-e who-stx))
                                       name))))]
      [_ #f]))))
