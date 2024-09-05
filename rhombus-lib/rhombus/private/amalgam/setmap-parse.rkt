#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     shrubbery/property
                     "tag.rkt"
                     "name-path-op.rkt")
         "parse.rkt"
         "static-info.rkt"
         "repetition.rkt"
         "compound-repetition.rkt"
         (submod "ellipsis.rkt" for-parse)
         "parens.rkt"
         "op-literal.rkt"
         "dotted-sequence-parse.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "key-comp.rkt")

(provide (for-syntax parse-setmap-content
                     build-setmap

                     flatten-setmap-arguments
                     regroup-setmap-arguments
                     parse-key-comp))

(begin-for-syntax
  (struct rest-rep (stx)))

;; parse-setmap-content :
;;   Syntax #:shape Shape #:who (U #f Symbol) -> (values Shape (Listof (U (Listof Stx) Stx)))
;; Parses the braces syntax of a set or map form into 2 values:
;;  * shape, which is one of
;;     - #f (compatible with both set and map)
;;     - 'set
;;     - 'map
;;  * argss, which is a list of arg lists and splices;
;     for a map shape, each arg list is an alternating list of keys and values;
;;    `rest-rep` values appear in the `argss` list to indicate a `...` splice
(define-for-syntax (parse-setmap-content stx
                                         #:set-for-form [set-for-form #f]
                                         #:map-for-form [map-for-form #'for/hashalw]
                                         #:shape [init-shape #f]
                                         #:who [who #f]
                                         #:raw? [raw? #f]
                                         #:repetition? [repetition? #f]
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
          (rest-rep (if repetition?
                        the-rep
                        (render-repetition set-for-form the-rep)))])]))
  (define (repetition-map-arguments key-e val-e extra-ellipses)
    (cond
      [raw? (list key-e val-e)]
      [else
       (define key-parsed (syntax-parse key-e [key::repetition #'key.parsed]))
       (define val-parsed (syntax-parse val-e [val::repetition #'val.parsed]))
       ;; This could be faster than building a list of pairs
       ;; to convert to a hash table...
       (define pair-rep (flatten-repetition
                         (build-compound-repetition
                          stx (list key-parsed val-parsed)
                          (lambda (key val)
                            (values #`(values #,key #,val)
                                    #'())))
                         extra-ellipses
                         #:pack-element (lambda (e)
                                          #`(let-values ([(a d) #,e])
                                              (cons a d)))
                         #:unpack-element (lambda (e)
                                            #`(let ([p #,e])
                                                (values (car p) (cdr p))))))
       (list (rest-rep (if repetition?
                           pair-rep
                           (render-repetition map-for-form pair-rep))))]))
  (syntax-parse stx
    #:datum-literals (group)
    [(_::braces elem ...)
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
              #:datum-literals (group op)
              [(group key-e ... (_::block val))
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
              #:datum-literals (group op)
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
              [(group key-e ... (_::block val))
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
                                 #:rep-for-form rep-for-form)
  (define (build argss)
    (for/fold ([base #f]
               #:result (or base
                            (quasisyntax/loc stx
                              (#,build-id))))
              ([args (in-list argss)])
      (cond
        [(list? args)
         ;; nonsplice arguments
         (if base
             (quasisyntax/loc stx
               (#,extend*-id #,base #,@args))
             (quasisyntax/loc stx
               (#,build-id #,@args)))]
        [else
         ;; a `&` or `...` splice
         (define e
           (cond
             [(rest-rep? args) (rest-rep-stx args)]
             [else #`(#,assert-id #,args)]))
         (if base
             (quasisyntax/loc stx
               (#,append-id #,base #,e))
             (if (rest-rep? args)
                 e
                 (quasisyntax/loc stx
                   (#,append-id (#,build-id) #,e))))])))
  (cond
    [repetition? (build-compound-repetition
                  stx
                  (flatten-setmap-arguments argss)
                  (lambda args
                    (values (build (regroup-setmap-arguments args argss))
                            static-info))
                  #:sequence-for-form rep-for-form
                  #:is-sequence? rest-rep?
                  #:extract (lambda (v) (if (rest-rep? v) (rest-rep-stx v) v)))]
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
      [(rest-rep? (car argss)) (cons (rest-rep (car args)) (loop (cdr args) (cdr argss)))]
      [else (cons (car args) (loop (cdr args) (cdr argss)))])))

(define-for-syntax (parse-key-comp stx k)
  (syntax-parse stx
    #:datum-literals (group)
    [(form (~and args (_::parens (group . name-seq::dotted-operator-or-identifier))) . tail)
     #:with (~var name(:hier-name-seq in-name-root-space in-key-comp-space name-path-op name-root-ref)) #'name-seq
     (define mapper (syntax-local-value* (in-key-comp-space #'name.name) key-comp-ref))
     (unless mapper (raise-syntax-error #f "not bound to a map configuration" stx #'name))
     (define str (format "~a(~a)" (syntax-e #'form) (syntax-e #'name.name)))
     (define new-form (syntax-raw-property
                       (datum->syntax #'form
                                      (string->symbol str)
                                      #'form
                                      #'form)
                       str))
     (k #`(#,new-form . tail) (list #'args) str mapper)]))
