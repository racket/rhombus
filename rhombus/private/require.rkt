#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/lexicon
                     "srcloc.rkt")
         "declaration.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         (only-in "assign.rkt"
                  [= rhombus=])
         (only-in "implicit.rkt"
                  #%literal))

(provide (rename-out [rhombus-require require])

         (for-space rhombus/require
                    #%literal
                    |.|
                    rename))

(begin-for-syntax
  (property require-prefix-operator prefix-operator)
  (property require-infix-operator infix-operator)

  (define in-require-space (make-interned-syntax-introducer 'rhombus/require))

  (define (check-require-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define (make-identifier-require id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'require
                          "not a valid module path element"
                          id))
    id)

  (define-enforest require-enforest require-enforest-step
    :require :require-prefix-op+form+tail :require-infix-op+form+tail
    "require" "require operator"
    in-require-space
    require-prefix-operator-ref require-infix-operator-ref
    check-require-result
    make-identifier-require)

  (define (extract-prefix r)
    (syntax-parse r
      [_:string (datum->syntax
                 r
                 (string->symbol
                  (regexp-replace #rx"[.].*$"
                                  (regexp-replace #rx"^.*/" (syntax-e r) "")
                                  ""))
                 r
                 r)]
      [_:identifier (datum->syntax
                     r
                     (string->symbol (regexp-replace #rx"^.*/"
                                                     (symbol->string (syntax-e r))
                                                     ""))
                     r
                     r)]
      [_ (raise-syntax-error 'require
                             "don't know how to extract default prefix"
                             r)]))

  (define-syntax-class :require-block
    #:datum-literals (block group op)
    #:literals (rhombus=)
    (pattern (group prefix:identifier (op rhombus=) req ...)
             #:with r::require #'(group req ...)
             #:attr parsed #'r.parsed)
    (pattern (group (op rhombus=) req ...)
             #:with r::require #'(group req ...)
             #:attr parsed #'r.parsed
             #:attr prefix #'#f)
    (pattern (group req ...)
             #:with r::require #'(group req ...)
             #:attr parsed #'r.parsed
             #:attr prefix (extract-prefix #'r.parsed))))

(define-syntax rhombus-require
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block r::require-block ...))
        (define prefixes (syntax->list #'(r.prefix ...)))
        (define intros (for/list ([prefix (in-list prefixes)])
                         (if (syntax-e prefix)
                             (make-syntax-introducer)
                             values)))
        (define parseds (for/list ([intro (in-list intros)]
                                   [r-parsed (syntax->list #'(r.parsed ...))])
                          (intro r-parsed)))
        (with-syntax ([(r-parsed ...) parseds]
                      [(def ...) (for/list ([prefix (in-list prefixes)]
                                            [parsed (in-list parseds)])
                                   (if (syntax-e prefix)
                                       #`(define-syntax #,prefix
                                           (lexicon (lambda (tail)
                                                      (parse-require-dot
                                                       (quote-syntax #,(datum->syntax parsed 'ctx))
                                                       tail))))
                                       #'(begin)))])
          #`((require r-parsed ...)
             def ...))]))))

(define-for-syntax (parse-require-dot ctx stxes)
  (define (get what name)
    (define id (datum->syntax ctx
                              (syntax-e name)
                              name
                              name))
    (unless (identifier-binding id)
      (raise-syntax-error #f
                          (format "no such required ~a" what)
                          name))
    id)
  (syntax-parse stxes
    #:datum-literals (op parens)
    #:literals (|.|)
    [(_ (op |.|) field:identifier . tail)
     (values (relocate #'field (get "identifier" #'field)) #'tail)]
    [(_ (op |.|) (parens (group (~and target (op field))))  . tail)
     (values (relocate #'target #`(op #,(get "operator" #'field))) #'tail)]))
  
(define-syntax (define-require-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-require-space #'name) rhs))]))

(define-require-syntax #%literal
  (require-prefix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ a . tail)
        (unless (module-path? (syntax->datum #'a))
          (raise-syntax-error 'require
                              "not a valid module path"
                              #'a))
        (values #'a
                #'tail)]))))

(define-require-syntax |.|
  (require-infix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (form1 stx)
     (unless (identifier? form1)
       (raise-syntax-error 'require
                           "not a valid module path element"
                           form1))
     (syntax-parse stx
       [(_ a . tail)
        (unless (and (identifier? #'a)
                     (module-path? (syntax->datum #'a)))
          (raise-syntax-error 'require
                              "not a valid module path element"
                              #'a))
        (values (datum->syntax #'a
                               (string->symbol
                                (format "~a/~a"
                                        (syntax-e form1)
                                        (syntax-e #'a)))
                               (span-srcloc form1 #'a)
                               #'a)
                #'tail)]))
   'left))

(define-require-syntax rename
  (require-infix-operator
   #'rename
   '((default . stronger))
   'macro
   (lambda (req stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block (group int:identifier #:to ext:identifier)
                  ...)
           . tail)
        (values (datum->syntax req
                               (list* #'rename-in req #'([int ext] ...))
                               req)
                #'tail)]))
   'none))
