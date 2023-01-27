#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/symbol)
         "space-clause.rkt"
         "space.rkt"
         "parens.rkt")

(provide (for-space rhombus/space_clause
                    space_path
                    macro_definer
                    meta_namespace))

(module+ for-space-macro
  (provide rhombus-space-clause
           (for-syntax parse-space-clause-options)))

(define-syntax rhombus-space-clause 'placeholder)

(define-for-syntax (wrap-clause parsed)
  #`[(group (parsed (quote-syntax (rhombus-space-clause #,parsed) #:local)))])

(begin-for-syntax
  (define-splicing-syntax-class :space_name
    #:attributes (name)
    #:datum-literals (/ op)
    (pattern (~seq root:identifier (~seq (op /) part:identifier) ...+)
             #:attr name (datum->syntax
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
       [(_ id:identifier)
        (wrap-clause #`(#:export_macro id))]))))

(define-space-clause-syntax meta_namespace
  (space-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ id:identifier (_::block . content))
        (wrap-clause #`(#:meta_namespace id content))]))))

(define-for-syntax (parse-space-clause-options orig-stx options-stx)
  (for/fold ([options #hasheq()]) ([option (syntax->list options-stx)])
    (syntax-parse option
      [(_ (#:space_path space-path-name orig-clause))
       (when (hash-ref options '#:space_path #f)
         (raise-syntax-error #f "multiple space paths declared" orig-stx #'orig-clause))
       (hash-set options '#:space_path #'space-path-name)]
      [(_ (#:export_macro define-macro))
       (when (hash-ref options '#:export_macro #f)
         (raise-syntax-error #f "multiple macro definer names declared" orig-stx #'define-macro))
       (hash-set options '#:export_macro #'define-macro)]
      [(_ (#:meta_namespace name content))
       (when (hash-ref options '#:meta_namespace #f)
         (raise-syntax-error #f "multiple meta namespaces declared" orig-stx #'name))
       (hash-set options '#:meta_namespace (cons #'name #'content))]
      [_ (error "unhandled option" option)])))
