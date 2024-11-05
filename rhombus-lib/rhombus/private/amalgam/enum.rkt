#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (submod "annotation.rkt" for-class)
         (submod "symbol.rkt" for-static-info)
         "name-root.rkt")

(provide define-simple-symbol-enum)

(define-syntax (define-simple-symbol-enum stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~seq #:extra [extra-field ...])
             (~seq))
        (~or* [val:id rkt-val:id]
              val:id)
        ...)
     #:with (val-name ...) (generate-temporaries #'(val ...))
     #:with name? (datum->syntax #'name (string->symbol
                                         (format "~a?" (syntax-e #'name))))
     #:attr ->name (and (not (null? (syntax-e #'((~? rkt-val) ...))))
                        (datum->syntax #'name (string->symbol
                                               (format "->~a" (syntax-e #'name)))))
     #'(begin
         (define val-name 'val) ...

         (define (name? v)
           (case v
             [(val) #t]
             ...
             [else #f]))

         (~? (define (->name v)
               (case v
                 [(val) '(~? rkt-val val)]
                 ...
                 [else #f])))

         (define-annotation-syntax name
           (identifier-annotation name? #,(get-symbol-static-infos)))

         (define-name-root name
           #:fields
           ([val val-name]
            ...
            (~? (~@ extra-field
                    ...)))))]))
