#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         (only-in "rest-marker.rkt" &))

(provide (for-syntax parse-setmap-content))

;; A Shape is one of:
;;  - #f
;;  - 'set
;;  - 'map

;; parse-setmap-content :
;;   Syntax #:shape Shape #:who (U #f Symbol) -> (values Shape (Listof Stx) (U #f Stx))
;; Parses the braces syntax of a set or map form into 3 values:
;;  * shape, where false means it's compatible with both set and map
;;  * args, which for a map shape is an alternating list of keys and values
;;  * maybe-rest, where false means there is no rest argument
(define-for-syntax (parse-setmap-content stx
                                         #:shape [init-shape #f]
                                         #:who [who #f])
  (syntax-parse stx
    #:datum-literals (block braces parens group)
    [(braces elem ...)
     (define-values (shape rev-args maybe-rest)
       (for/fold ([shape init-shape] [args '()] [rst #f])
                 ([elem (in-list (syntax->list #'(elem ...)))])
         (when rst
           (raise-syntax-error who "element after rest" stx elem))
         (syntax-parse elem
           #:datum-literals (block braces parens group op)
           #:literals (&)
           [(group (op &) new-rst ...)
            (values shape
                    args
                    #'(rhombus-expression (group new-rst ...)))]
           [(group key-e ... (block val))
            #:when (not (eq? init-shape 'set))
            (when (eq? shape 'set)
              (raise-syntax-error #f "map element after set element" stx elem))
            (values 'map
                    (list* #'(rhombus-expression val)
                           #'(rhombus-expression (group key-e ...))
                           args)
                    rst)]
           [_
            (when (eq? init-shape 'map)
              (raise-syntax-error who "element must be `<key> : <value>`" stx elem))
            (when (eq? shape 'map)
              (raise-syntax-error who "set element after map element" stx elem))
            (values 'set
                    (cons #`(rhombus-expression #,elem) args)
                    rst)])))
     (values shape (reverse rev-args) maybe-rest)]))
