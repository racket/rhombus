#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt")

(provide (for-syntax parse-setmap-content))

(define-for-syntax (parse-setmap-content stx
                                         #:shape [init-shape #f]
                                         #:who [who #f])
  (syntax-parse stx
    #:datum-literals (block braces parens group)
    [(braces elem ...)
     (define-values (shape rev-args)
       (for/fold ([shape init-shape] [args '()]) ([elem (in-list (syntax->list #'(elem ...)))])
         (define-values (new-shape new-args)
           (syntax-parse elem
             #:datum-literals (block braces parens group)
             [(group key-e ... (block val))
              #:when (not (eq? init-shape 'set))
              (values 'map
                      (list* #'(rhombus-expression val)
                             #'(rhombus-expression (group key-e ...))
                             args))]
             [_ (values 'set (cons #`(rhombus-expression #,elem) args))]))
         (when (and shape (not (eq? shape new-shape)))
           (raise-syntax-error who
                               (if (eq? shape 'set)
                                   "set element after map element"
                                   (if (not init-shape)
                                       "map element after set element"
                                       "element must be `<key> : <value>`"))
                               stx
                               elem))
         (values new-shape new-args)))
     (values shape (reverse rev-args))]))
