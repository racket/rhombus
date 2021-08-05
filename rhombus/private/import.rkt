#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest
                     enforest/operator
                     enforest/property
                     enforest/proc-name
                     enforest/lexicon
                     "srcloc.rkt"
                     "hierarchy-op.rkt")
         "declaration.rkt"
         "dot.rkt"
         (submod "dot.rkt" for-dot-provider)
         (only-in "assign.rkt"
                  [= rhombus=])
         (only-in "implicit.rkt"
                  #%literal))

(provide import

         (for-space rhombus/import
                    #%literal
                    |.|
                    rename))

(begin-for-syntax
  (property import-prefix-operator prefix-operator)
  (property import-infix-operator infix-operator)

  (define in-import-space (make-interned-syntax-introducer 'rhombus/import))

  (define (check-import-result form proc)
    (unless (syntax? form) (raise-result-error (proc-name proc) "syntax?" form))
    form)

  (define (make-identifier-import id)
    (unless (module-path? (syntax-e id))
      (raise-syntax-error 'import
                          "not a valid module path element"
                          id))
    id)

  (define-enforest import-enforest import-enforest-step
    :import :import-prefix-op+form+tail :import-infix-op+form+tail
    "import" "import operator"
    in-import-space
    hierarchy-op import-prefix-operator-ref import-infix-operator-ref
    check-import-result
    make-identifier-import)

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
      [((~literal rename-in) mp . _) (extract-prefix #'mp)]
      [_ (raise-syntax-error 'import
                             "don't know how to extract default prefix"
                             r)]))

  (define-syntax-class :import-block
    #:datum-literals (block group op)
    #:literals (rhombus=)
    (pattern (group prefix:identifier (op rhombus=) req ...)
             #:with r::import #'(group req ...)
             #:attr parsed #'r.parsed)
    (pattern (group (op rhombus=) req ...)
             #:with r::import #'(group req ...)
             #:attr parsed #'r.parsed
             #:attr prefix #'#f)
    (pattern (group req ...)
             #:with r::import #'(group req ...)
             #:attr parsed #'r.parsed
             #:attr prefix (extract-prefix #'r.parsed))))

(define-syntax import
  (declaration-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (block)
       [(_ (block r::import-block ...))
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
                                                      (parse-import-dot
                                                       (quote-syntax #,(datum->syntax parsed 'ctx))
                                                       tail))))
                                       #'(begin)))])
          #`((require r-parsed ...)
             def ...))]))))

(define-for-syntax (parse-import-dot ctx stxes)
  (define (get what name)
    (define id (datum->syntax ctx
                              (syntax-e name)
                              name
                              name))
    (unless (identifier-binding id)
      (raise-syntax-error #f
                          (format "no such importd ~a" what)
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
                         #'form-id)]))
  
(define-syntax (define-import-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-import-space #'name) rhs))]))

(define-import-syntax #%literal
  (import-prefix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ a . tail)
        (unless (module-path? (syntax->datum #'a))
          (raise-syntax-error 'import
                              "not a valid module path"
                              #'a))
        (values #'a
                #'tail)]))))

(define-import-syntax |.|
  (import-infix-operator
   #'%literal
   '((default . stronger))
   'macro
   (lambda (form1 stx)
     (unless (identifier? form1)
       (raise-syntax-error 'import
                           "not a valid module path element"
                           form1))
     (syntax-parse stx
       [(_ a . tail)
        (unless (and (identifier? #'a)
                     (module-path? (syntax->datum #'a)))
          (raise-syntax-error 'import
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

(define-import-syntax rename
  (import-infix-operator
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
