#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol)
         "space-clause.rkt"
         "parens.rkt")

(provide (for-space rhombus/space_clause
                    space_path
                    macro_definer
                    bridge_definer
                    meta_namespace
                    private))

(module+ for-space-macro
  (provide rhombus-space-clause
           (for-syntax parse-space-clause-options)))

(define-syntax rhombus-space-clause 'placeholder)

(define-for-syntax (wrap-clause parsed)
  #`[(group (parsed #:rhombus/space_clause (quote-syntax (rhombus-space-clause #,parsed) #:local)))])
(define-for-syntax (unwrap-clause parsed)
  (syntax-parse parsed
    #:datum-literals (group parsed quote-syntax rhombus-space-clause)
    [[(group (parsed #:rhombus/space_clause (quote-syntax (rhombus-space-clause p) #:local)))]
     #'p]))

(begin-for-syntax
  (define-splicing-syntax-class :space_name
    #:attributes (name)
    #:datum-literals (/ op)
    (pattern (~seq root:identifier (~seq (op /) part:identifier) ...+)
             #:with name (datum->syntax
                          #f
                          (string->symbol
                           (apply string-append
                                  (id->string #'root)
                                  (map id->string
                                       (syntax->list #'((~@ / part) ...)))))))
    (pattern (~seq name:identifier)))

  (define (id->string s) (symbol->immutable-string (syntax-e s))))

(define-space-clause-syntax space_path
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ space-path::space_name)
        (wrap-clause #`(#:space_path space-path.name #,stx))]))))

(define-space-clause-syntax macro_definer
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ id:identifier)
        (wrap-clause #`(#:export_macro id #t))]
       [(_ id:identifier
           (_::block
            (group kw:keyword)
            ...))
        (wrap-clause #`(#:export_macro id #t kw ...))]))))

(define-space-clause-syntax bridge_definer
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier)
        (wrap-clause #`(#:export_bridge id #t))]))))

(define-space-clause-syntax meta_namespace
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier (_::block . content))
        (wrap-clause #`(#:meta_namespace id content))]))))

(define-space-clause-syntax private
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier . rest)
        #:when (let ([id (in-space-clause-space #'id)])
                 (or (free-identifier=? id (in-space-clause-space (quote-syntax macro_definer)))
                     (free-identifier=? id (in-space-clause-space (quote-syntax bridge_definer)))))
        (syntax-parse #'(group id . rest)
          [cl::space-clause
           (syntax-parse (unwrap-clause #'cl.parsed)
             [(kw id #t . rest)
              (wrap-clause #'(kw id #f . rest))])])]
       [else
        (raise-syntax-error #f
                            "expected macro definer, bridge definer, or namespace clause to make private"
                            stx)]))))

(define-for-syntax (parse-space-clause-options orig-stx options-stx)
  (define (maybe-private options public?-stx kw)
    (if (syntax-e public?-stx)
        options
        (hash-set options '#:private (hash-set (hash-ref options #'#:private #hasheq()) kw #t))))
  (for/fold ([options #hasheq()]) ([option (in-list (syntax->list options-stx))])
    (syntax-parse option
      [(_ (#:space_path space-path-name orig-clause))
       (when (hash-ref options '#:space_path #f)
         (raise-syntax-error #f "multiple space paths declared" orig-stx #'orig-clause))
       (hash-set options '#:space_path #'space-path-name)]
      [(_ (#:export_macro define-macro public? kw ...))
       (when (hash-ref options '#:export_macro #f)
         (raise-syntax-error #f "multiple macro definer names declared" orig-stx #'define-macro))
       (maybe-private (hash-set (hash-set options '#:export_macro #'define-macro)
                                '#:export_macro_keywords (syntax->list #'(kw ...)))
                      #'public? '#:export_macro)]
      [(_ (#:export_bridge define-bridge public?))
       (when (hash-ref options '#:export_bridge #f)
         (raise-syntax-error #f "multiple bridge definer names declared" orig-stx #'define-bridge))
       (maybe-private (hash-set options '#:export_bridge #'define-bridge)
                      #'public?
                      '#:export_bridge)]
      [(_ (#:meta_namespace name content))
       (when (hash-ref options '#:meta_namespace #f)
         (raise-syntax-error #f "multiple meta namespaces declared" orig-stx #'name))
       (hash-set options '#:meta_namespace (cons #'name #'content))]
      [_ (error "unhandled option" option)])))
