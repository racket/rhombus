#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     (prefix-in enforest: enforest/name-root)
                     "srcloc.rkt")
         "dot.rkt")

;; convert a hierachical layer implemented as portal syntax to a name-root

(provide (for-syntax name-root-ref))

(define-for-syntax (name-root-ref v)
  (or
   (enforest:name-root-ref v)
   (and
    (portal-syntax? v)
    (syntax-parse (portal-syntax-content v)
      [([(~datum import) _] pre-ctx-s ctx-s)
       (define pre-ctx #'pre-ctx-s)
       (define ctx #'ctx-s)
       (enforest:name-root
        (lambda (stxes)
          (define (get what name)
            (define id (datum->syntax ctx
                                      (syntax-e name)
                                      name
                                      name))
            (define pre-id (datum->syntax pre-ctx (syntax-e name)))
            (unless (identifier-distinct-binding id pre-id)
              (raise-syntax-error #f
                                  (format "no such imported ~a" what)
                                  name))
            id)
          (syntax-parse stxes
            #:datum-literals (op parens)
            #:literals (|.|)
            [(_ (op |.|) field:identifier . tail)
             (values (relocate #'field (get "identifier" #'field)) #'tail)]
            [(_ (op |.|) (parens (group (~and target (op field))))  . tail)
             (values (relocate #'target #`(op #,(get "operator" #'field))) #'tail)]
            [(form-id (op (~and dot |.|)) . tail)
             (raise-syntax-error #f
                                 "expected an identifier or parentheses after dot"
                                 #'dot)]
            [(form-id . tail)
             (raise-syntax-error #f
                                 "expected a dot after import name"
                                 #'form-id)])))]
      [_ #f]))))
