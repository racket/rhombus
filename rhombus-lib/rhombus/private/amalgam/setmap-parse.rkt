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
         "key-comp.rkt"
         "sequence-element-key.rkt"
         "values-key.rkt")

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
      [raw? (values e #f)]
      [repetition? (syntax-parse e [rep::repetition
                                    (syntax-parse #'rep.parsed
                                      [info::repetition-info
                                       (values #'rep.parsed #'info.element-static-infos)])])]
      [else
       (define e-parsed (rhombus-local-expand #`(rhombus-expression #,e)))
       (values e-parsed
               (extract-static-infos e-parsed))]))
  (define (repetition-set-argument e extra-ellipses)
    (cond
      [raw? (values e #f)]
      [else
       (syntax-parse e
         [rep::repetition
          (define the-rep (flatten-repetition #'rep.parsed extra-ellipses))
          (values
           (rest-rep (if repetition?
                         the-rep
                         (render-repetition set-for-form the-rep)))
           (syntax-parse #'rep.parsed [elem::repetition-info #'elem.element-static-infos]))])]))
  (define (repetition-map-arguments key-e val-e extra-ellipses)
    (cond
      [raw? (values (list key-e val-e) #f #f)]
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
       (values
        (list (rest-rep (if repetition?
                            pair-rep
                            (render-repetition map-for-form pair-rep))))
        (syntax-parse key-parsed [key::repetition-info #'key.element-static-infos])
        (syntax-parse val-parsed [val::repetition-info #'val.element-static-infos]))]))
  (define (extract-splice-static-infos e-static-infos shape elems)
    (cond
      [(not shape)
       (define inferred-shape
         (for/or ([elem (in-list elems)])
           (syntax-parse elem
             #:datum-literals (group op)
             [(group and-op::&-expr new-rst ...) #f]
             [(group key-e ... (_::block . _)) 'map]
             [_ 'set])))
       (extract-splice-static-infos e-static-infos (or inferred-shape 'map) elems)]
      [else
       (define si (static-info-lookup e-static-infos #'#%sequence-element))
       (cond
         [(not si) (values #'() #'())]
         [(eq? shape 'set) (values si #f)]
         [else
          (syntax-parse (static-info-lookup si #'#%values)
            [(k v) (values #'k #'v)]
            [_ (values #'() #'())])])]))
  (syntax-parse stx
    #:datum-literals (group)
    [(_::braces elem ...)
     (define-values (shape rev-args rev-argss key-static-infos val-static-infos)
       (let loop ([elems (syntax->list #'(elem ...))]
                  [shape init-shape]
                  [rev-args '()]
                  [rev-argss '()]
                  [key-static-infos #f]
                  [val-static-infos #f])
         (define (assert-map)
           (when (eq? shape 'set)
             (raise-syntax-error #f "map element after set element" stx (car elems))))
         (define (assert-set)
           (when (eq? init-shape 'map)
             (raise-syntax-error who "element must be `<key> : <value>`" stx (car elems)))
           (when (eq? shape 'map)
             (raise-syntax-error who "set element after map element" stx (car elems))))
         (define (static-info-combine static-infos new-static-infos)
           (if (not static-infos)
               new-static-infos
               (static-infos-or static-infos new-static-infos)))
         (cond
           [(null? elems) (values shape rev-args rev-argss key-static-infos val-static-infos)]
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
               (define-values (es e-key-static-infos e-val-static-infos)
                 (repetition-map-arguments #`(#,group-tag key-e ...) #'val extra-ellipses))
               (loop (list-tail (cddr elems) extra-ellipses)
                     'map
                     '()
                     (append es
                             (if (null? rev-args)
                                 rev-argss
                                 (cons (reverse rev-args) rev-argss)))
                     (static-info-combine key-static-infos e-key-static-infos)
                     (static-info-combine val-static-infos e-val-static-infos))]
              [(group key-e ... (~and blk (_::block val ...)))
               #:when (not (eq? init-shape 'set))
               (raise-syntax-error who "repetition requires a single-group block" #'blk)]
              [_
               (assert-set)
               (define-values (e e-static-infos)
                 (repetition-set-argument elem extra-ellipses))
               (loop (list-tail (cddr elems) extra-ellipses)
                     'set
                     '()
                     (cons e
                           (if (null? rev-args)
                               rev-argss
                               (cons (reverse rev-args) rev-argss)))
                     (static-info-combine key-static-infos e-static-infos)
                     #f)])]
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
               (define-values (e e-static-infos) (one-argument #'(group new-rst ...)))
               (define-values (e-key-static-infos e-val-static-infos)
                 (if e-static-infos
                     (extract-splice-static-infos e-static-infos shape (cdr elems))
                     (values #f #f)))
               (loop (cdr elems)
                     shape
                     '()
                     (cons e
                           (if (null? rev-args)
                               rev-argss
                               (cons (reverse rev-args) rev-argss)))
                     (static-info-combine key-static-infos e-key-static-infos)
                     (static-info-combine val-static-infos e-val-static-infos))]
              [(group key-e ... (_::block val))
               #:when (not (eq? init-shape 'set))
               (assert-map)
               (define-values (e-k e-key-static-infos) (one-argument #'(group key-e ...)))
               (define-values (e-v e-val-static-infos) (one-argument #'val))
               (loop (cdr elems)
                     'map
                     (list* e-v
                            e-k
                            rev-args)
                     rev-argss
                     (static-info-combine key-static-infos e-key-static-infos)
                     (static-info-combine val-static-infos e-val-static-infos))]
              [(group key-e ... (~and blk (b-tag::block val ...)))
               #:when (not (eq? init-shape 'set))
               (when repetition?
                 (raise-syntax-error who "repetition requires a single-group block" #'blk))
               (assert-map)
               (define-values (e-k e-key-static-infos) (one-argument #'(group key-e ...)))
               (loop (cdr elems)
                     'map
                     (list* #`(rhombus-body-at b-tag val ...)
                            e-k
                            rev-args)
                     rev-argss
                     (static-info-combine key-static-infos e-key-static-infos)
                     #'())]
              [_
               (assert-set)
               (define-values (e e-static-infos) (one-argument elem))
               (loop (cdr elems)
                     'set
                     (cons e rev-args)
                     rev-argss
                     (static-info-combine key-static-infos e-static-infos)
                     #f)])])))
     (values shape
             (if (null? rev-args)
                 (reverse rev-argss)
                 (reverse (cons (reverse rev-args) rev-argss)))
             (or key-static-infos #'())
             (or val-static-infos #'()))]))

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
               (#,extend*-id #,base #,@(map discard-static-infos args)))
             (quasisyntax/loc stx
               (#,build-id #,@(map discard-static-infos args))))]
        [else
         ;; a `&` or `...` splice
         (define e
           (cond
             [(rest-rep? args) (rest-rep-stx args)]
             [else #`(#,assert-id #,(discard-static-infos args))]))
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
    [(form (~and args (p-tag::parens (group . name-seq::dotted-operator-or-identifier))) . tail)
     #:with (~var name(:hier-name-seq in-name-root-space in-key-comp-space name-path-op name-root-ref)) #'name-seq
     (define mapper (syntax-local-value* (in-key-comp-space #'name.name) key-comp-ref))
     (unless mapper (raise-syntax-error #f "not bound to a map configuration" stx #'name))
     (define str (format "~a(~a)" (syntax-e #'form) (syntax-e #'name.name)))
     (define new-form (syntax-raw-suffix-property
                       (syntax-raw-property
                        (datum->syntax #'form
                                       (string->symbol str)
                                       #'form
                                       #'form)
                        str)
                       (syntax-raw-suffix-property #'p-tag)))
     (k #`(#,new-form . tail) (list #'args) str mapper)]))
