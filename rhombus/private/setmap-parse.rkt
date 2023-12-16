#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         "parse.rkt"
         "static-info.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "ellipsis.rkt" for-parse)
         "op-literal.rkt")

(provide (for-syntax parse-setmap-content
                     build-setmap

                     flatten-setmap-arguments
                     regroup-setmap-arguments))

;; A Shape is one of:
;;  - #f
;;  - 'set
;;  - 'map

(begin-for-syntax
  (struct inlined (rep)))

;; parse-setmap-content :
;;   Syntax #:shape Shape #:who (U #f Symbol) -> (values Shape (Listof (U (Listof Stx) Stx)))
;; Parses the braces syntax of a set or map form into 2 values:
;;  * shape, where false means it's compatible with both set and map
;;  * argss, which is a list of arg lists and splices;
;     for a map shape, each arg list is an alternating list of keys and values;
;;    `inlined` values appear in the `argss` list for a repetition to be spliced
;;    when generating a new repetition
(define-for-syntax (parse-setmap-content stx
                                         #:shape [init-shape #f]
                                         #:who [who #f]
                                         #:raw? [raw? #f]
                                         #:repetition? [repetition? #f]
                                         #:list->set [list->set #f]
                                         #:list->map [list->map #f]
                                         #:no-splice [no-splice #f])
  (define (one-argument e)
    (cond
      [raw? e]
      [repetition? (syntax-parse e [rep::repetition #'rep.parsed])]
      [else #`(rhombus-expression #,e)]))
  (define (repetition-set-argument e extra-ellipses)
    (cond
      [raw? e]
      [else
       (syntax-parse e
         [rep::repetition
          (define the-rep (flatten-repetition #'rep.parsed extra-ellipses))
          (if repetition?
              (inlined the-rep)
              #`(#,list->set #,(repetition-as-list the-rep 1)))])]))
  (define (repetition-map-arguments key-e val-e extra-ellipses)
    (cond
      [raw? (list key-e val-e)]
      [else
       (define key-parsed (syntax-parse key-e [key::repetition #'key.parsed]))
       (define val-parsed (syntax-parse val-e [val::repetition #'val.parsed]))
       ;; This could be faster than building list of pair
       ;; to convert to a hash table...
       (define pair-rep (flatten-repetition
                         (build-compound-repetition
                          stx (list key-parsed val-parsed)
                          (lambda (key val)
                            (values #`(cons #,key #,val)
                                    #'())))
                         extra-ellipses))
       (if repetition?
           (list (inlined pair-rep))
           (list #`(#,list->map #,(repetition-as-list pair-rep 1))))]))
  (syntax-parse stx
    #:datum-literals (block braces parens group)
    [(braces elem ...)
     (define-values (shape rev-args rev-argss)
       (let loop ([elems (syntax->list #'(elem ...))]
                  [shape init-shape]
                  [rev-args '()]
                  [rev-argss '()])
         (define (assert-map)
           (when (eq? shape 'set)
             (raise-syntax-error #f "map element after set element" stx (car elems))))
         (define (assert-set)
           (when (eq? init-shape 'map)
             (raise-syntax-error who "element must be `<key> : <value>`" stx (car elems)))
           (when (eq? shape 'map)
             (raise-syntax-error who "set element after map element" stx (car elems))))
         (cond
           [(null? elems) (values shape rev-args rev-argss)]
           [(and (pair? (cdr elems))
                 (syntax-parse (cadr elems)
                   #:datum-literals (group op)
                   [(group (op dots-op::...-expr))
                    (when no-splice
                      (raise-syntax-error #f
                                          (format "repetition splicing is not supported on ~a" no-splice)
                                          #'dots-op.name))
                    #t]
                   [_ #f]))
            ;; repetition
            (define elem (car elems))
            (define-values (ignored-gs extra-ellipses) (consume-extra-ellipses (cddr elems)))
            (syntax-parse elem
              #:datum-literals (block braces parens group op)
              [(group key-e ... (block val))
               #:when (not (eq? init-shape 'set))
               (assert-map)
               (loop (list-tail (cddr elems) extra-ellipses)
                     'map
                     '()
                     (append (repetition-map-arguments #`(#,group-tag key-e ...) #'val extra-ellipses)
                             (if (null? rev-args)
                                 rev-argss
                                 (cons (reverse rev-args) rev-argss))))]
              [_
               (assert-set)
               (loop (list-tail (cddr elems) extra-ellipses)
                     'set
                     '()
                     (cons (repetition-set-argument elem extra-ellipses)
                           (if (null? rev-args)
                               rev-argss
                               (cons (reverse rev-args) rev-argss))))])]
           [else
            ;; single element or splice
            (define elem (car elems))
            (syntax-parse elem
              #:datum-literals (block braces parens group op)
              [(group and-op::&-expr new-rst ...)
               (when no-splice
                 (raise-syntax-error #f
                                     (format "`& rest` is not supported on ~a" no-splice)
                                     #'and-op.name))
               (loop (cdr elems)
                     shape
                     '()
                     (cons (one-argument #'(group new-rst ...))
                           (if (null? rev-args)
                               rev-argss
                               (cons (reverse rev-args) rev-argss))))]
              [(group key-e ... (block val))
               #:when (not (eq? init-shape 'set))
               (assert-map)
               (loop (cdr elems)
                     'map
                     (list* (one-argument #'val)
                            (one-argument #'(group key-e ...))
                            rev-args)
                     rev-argss)]
              [_
               (assert-set)
               (loop (cdr elems)
                     'set
                     (cons (one-argument elem) rev-args)
                     rev-argss)])])))
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
                                 static-info
                                 #:repetition? [repetition? #f]
                                 #:list->setmap [list->setmap #f])
  (define (build argss)
    (let loop ([base #f] [argss argss])
      (cond
        [(null? argss)
         (or base
             (quasisyntax/loc stx (#,build-id)))]
        [(list? (car argss))
         ;; nonsplice arguments
         (loop (if base
                   (quasisyntax/loc stx
                     (#,extend*-id #,base #,@(car argss)))
                   (quasisyntax/loc stx
                     (#,build-id #,@(car argss))))
               (cdr argss))]
        [else
         ;; a `&` or `...` splice
         (define e (let ([e (car argss)])
                     (if (inlined? e)
                         #`(#,list->setmap #,(inlined-rep e))
                         e)))
         (loop (if base
                   (quasisyntax/loc stx
                     (#,append-id #,base #,e))
                   (if (inlined? e)
                       e
                       (quasisyntax/loc stx
                         (#,assert-id #,e))))
               (cdr argss))])))
  (cond
    [repetition? (build-compound-repetition
                  stx
                  (flatten-setmap-arguments argss)
                  (lambda args
                    (values (build (regroup-setmap-arguments args argss))
                            static-info))
                  #:is-sequence? inlined?
                  #:extract (lambda (v) (if (inlined? v) (inlined-rep v) v)))]
    [else (wrap-static-info* (build argss) static-info)]))

;; flatten arguments that represent normal arguments and splices
(define-for-syntax (flatten-setmap-arguments argss)
  (let loop ([argss argss])
    (cond
      [(null? argss) '()]
      [(list? (car argss)) (append (car argss) (loop (cdr argss)))]
      [else (cons (car argss) (loop (cdr argss)))])))

;; un-flatten arguments to match the `argss` pattern of normal arguments and splices
(define-for-syntax (regroup-setmap-arguments args argss)
  (let loop ([args args] [argss argss])
    (cond
      [(null? args) '()]
      [(list? (car argss))
       (let aloop ([args args] [one-args (car argss)] [rev-args '()])
         (cond
           [(null? one-args) (cons (reverse rev-args)
                                   (loop args (cdr argss)))]
           [else (aloop (cdr args) (cdr one-args) (cons (car args) rev-args))]))]
      [(inlined? (car argss)) (cons (inlined (car args)) (loop (cdr args) (cdr argss)))]
      [else (cons (car args) (loop (cdr args) (cdr argss)))])))
