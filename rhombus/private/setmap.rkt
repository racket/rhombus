#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "map.rkt"
         (submod "map.rkt" for-info)
         "set.rkt"
         (submod "set.rkt" for-info)
         "static-info.rkt"
         "parse.rkt")

(provide (for-syntax parse-setmap-expression))

(define-for-syntax (parse-setmap-expression stx)
  (syntax-parse stx
    #:datum-literals (block braces parens group)
    [(braces elem ...)
     (define-values (shape rev-args)
       (for/fold ([shape #f] [args '()]) ([elem (in-list (syntax->list #'(elem ...)))])
         (define-values (new-shape new-args)
           (syntax-parse elem
             #:datum-literals (block braces parens group)
             [(group key-e ... (block val)) (values 'map
                                                (list* #'(rhombus-expression val)
                                                       #'(rhombus-expression (group key-e ...))
                                                       args))]
             [_ (values 'set (cons #`(rhombus-expression #,elem) args))]))
         (when (and shape (not (eq? shape new-shape)))
           (raise-syntax-error #f
                               (if (eq? shape 'set)
                                   "set element after map element"
                                   "map element after set element")
                               stx
                               elem))
         (values new-shape new-args)))
     (wrap-static-info*
      (quasisyntax/loc stx
        (#,(if (eq? shape 'set) #'Set #'Map)
         #,@(reverse rev-args)))
      (if (eq? shape 'set)
          set-static-info
          map-static-info))]))
