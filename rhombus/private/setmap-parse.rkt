#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         (only-in "rest-marker.rkt" &)
         "static-info.rkt")

(provide (for-syntax parse-setmap-content
                     build-setmap))

;; A Shape is one of:
;;  - #f
;;  - 'set
;;  - 'map

;; parse-setmap-content :
;;   Syntax #:shape Shape #:who (U #f Symbol) -> (values Shape (Listof (U (Listof Stx) Stx)))
;; Parses the braces syntax of a set or map form into 2 values:
;;  * shape, where false means it's compatible with both set and map
;;  * argss, which is a list of arg lists and splices;
;     for a map shape, each arg list is an alternating list of keys and values
(define-for-syntax (parse-setmap-content stx
                                         #:shape [init-shape #f]
                                         #:who [who #f])
  (syntax-parse stx
    #:datum-literals (block braces parens group)
    [(braces elem ...)
     (define-values (shape rev-args rev-argss)
       (for/fold ([shape init-shape] [rev-args '()] [rev-argss '()])
                 ([elem (in-list (syntax->list #'(elem ...)))])
         (syntax-parse elem
           #:datum-literals (block braces parens group op)
           #:literals (&)
           [(group (op &) new-rst ...)
            (values shape
                    '()
                    (cons #'(rhombus-expression (group new-rst ...))
                          (if (null? rev-args)
                              rev-argss
                              (cons (reverse rev-args) rev-argss))))]
           [(group key-e ... (block val))
            #:when (not (eq? init-shape 'set))
            (when (eq? shape 'set)
              (raise-syntax-error #f "map element after set element" stx elem))
            (values 'map
                    (list* #'(rhombus-expression val)
                           #'(rhombus-expression (group key-e ...))
                           rev-args)
                    rev-argss)]
           [_
            (when (eq? init-shape 'map)
              (raise-syntax-error who "element must be `<key> : <value>`" stx elem))
            (when (eq? shape 'map)
              (raise-syntax-error who "set element after map element" stx elem))
            (values 'set
                    (cons #`(rhombus-expression #,elem) rev-args)
                    rev-argss)])))
     (values shape
             (if (null? rev-args)
                 (reverse rev-argss)
                 (reverse (cons (reverse rev-args) rev-argss))))]))

(define-for-syntax (build-setmap stx
                                 argss
                                 build-id
                                 extend*-id
                                 append-id
                                 assert-id
                                 static-info)
  (define built
    (let loop ([base #f] [argss argss])
      (cond
        [(null? argss)
         (or base
             (quasisyntax/loc stx (#,build-id)))]
        [(list? (car argss))
         (loop (if base
                   (quasisyntax/loc stx
                     (#,extend*-id #,base #,@(car argss)))
                   (quasisyntax/loc stx
                     (#,build-id #,@(car argss))))
               (cdr argss))]
        [else
         (loop (if base
                   (quasisyntax/loc stx
                     (#,append-id #,base #,(car argss)))
                   (quasisyntax/loc stx
                     (#,assert-id #,(car argss))))
               (cdr argss))])))
  (wrap-static-info* built static-info))
