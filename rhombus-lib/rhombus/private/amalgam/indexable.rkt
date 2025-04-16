#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt"
                     "class-method-result.rkt"
                     "annot-context.rkt")
         "treelist.rkt"
         "mutable-treelist.rkt"
         "provide.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         "index-key.rkt"
         "index-result-key.rkt"
         "call-result-key.rkt"
         "static-info.rkt"
         (submod "assign.rkt" for-assign)
         "repetition.rkt"
         "compound-repetition.rkt"
         "index-property.rkt"
         "mutability.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "parens.rkt"
         (submod "map-maybe.rkt" for-print)
         (only-in (submod "function-parse.rkt" for-build)
                  find-call-result-at))

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     Indexable
                     MutableIndexable))

(module+ for-ref
  (provide (for-syntax parse-indexable-ref-or-set)))

(define-values (prop:Indexable Indexable? Indexable-ref)
  (make-struct-type-property 'Indexable
                             #f
                             ;; could have `prop:index` in this list, but
                             ;; direct dispatch is set up in `class` when
                             ;; the interface is implemented, and that
                             ;; picks up the right static info
                             (list)))

(define-values (prop:MutableIndexable MutableIndexable? MutableIndexable-ref)
  (make-struct-type-property 'Indexable
                             #f
                             ;; similarly, could have `prop:setable` in
                             ;; this list, but direct dispatch is better
                             (list)))

(define-annotation-syntax Indexable
  (identifier-annotation indexable? ((#%index-get indexable-get))))
(define (indexable? v)
  (or (treelist? v)
      (list? v)
      (vector? v)
      (hash? v)
      (string? v)
      (bytes? v)
      (mutable-treelist? v)
      (map-maybe? v)
      (Indexable? v)))

(define-class-desc-syntax Indexable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&get)
                     #'#(#:abstract)
                     (hasheq 'get 0)
                     (hasheq 'get #'get-result)
                     '()
                     #f
                     #'()
                     '(get veneer)
                     ;; --------------------
                     #'Indexable
                     #'Indexable
                     #'prop:Indexable
                     #'prop:Indexable
                     #'Indexable-ref
                     #'Indexable-ref
                     #t
                     #f
                     null))))

(define-syntax get-result
  (method-result-maker
   (lambda ()
     (method-result #'(lambda (x) #t) #t 1 "Any" #'() 4))))

(define-annotation-syntax MutableIndexable
  (identifier-annotation mutable-indexable? ((#%index-get indexable-get)
                                             (#%index-set indexable-set!))))
(define (mutable-indexable? v)
  (or (mutable-vector? v)
      (mutable-hash? v)
      (mutable-bytes? v)
      (mutable-treelist? v)
      (MutableIndexable? v)))

(define-class-desc-syntax MutableIndexable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'(Indexable)
                     '#(#&get #&set)
                     #'#(#:abstract #:abstract)
                     (hasheq 'get 0
                             'set 1)
                     (hasheq 'set #'set-result)
                     '()
                     #f
                     #'()
                     '(get set veneer)
                     ;; --------------------
                     #'MutableIndexable
                     #'MutableIndexable
                     #'prop:MutableIndexable
                     #'prop:MutableIndexable
                     #'MutableIndexable-ref
                     #'MutableIndexable-ref
                     #t
                     #f
                     null))))

(define-syntax set-result
  (method-result-maker
   (lambda ()
     (method-result #'void? #t 1 "Void" #'() 8))))

(define-for-syntax (parse-indexable-ref-or-set indexable-in stxes more-static?
                                               #:repetition? [repetition? #f])
  (define indexable (if repetition?
                        indexable-in
                        (rhombus-local-expand indexable-in)))
  (define who '|[]|)
  (define (not-static) (string-append "specialization not known" statically-str))
  (syntax-parse stxes
    #:datum-literals (op)
    [(_ (_::brackets index) . assign-tail)
     #:when (not repetition?)
     #:with assign::assign-op-seq #'assign-tail
     (define op (attribute assign.op))
     (define indexable-set!-id (or (syntax-local-static-info indexable #'#%index-set)
                                   (if more-static?
                                       (raise-syntax-error who (not-static) indexable-in)
                                       #'indexable-set!)))
     (define indexable-ref-id (or (syntax-local-static-info indexable #'#%index-get)
                                  (if more-static?
                                      (raise-syntax-error who (not-static) indexable-in)
                                      #'indexable-get)))
     (define-values (assign-expr tail) (build-assign
                                        op
                                        #'assign.op-name
                                        #'assign.name
                                        #`(lambda () (#,indexable-ref-id indexable-v index-v))
                                        #`(lambda (v) (#,indexable-set!-id indexable-v index-v v))
                                        #'indexable
                                        #'assign.tail))
     (values #`(let ([indexable-v #,indexable]
                     [index-v (rhombus-expression index)])
                 #,assign-expr)
             tail)]
    [(_ (~and args (head::brackets index)) . tail)
     (define (build-ref indexable index indexable-static-info indexable-static-infos index-static-infos)
       (define indexable-ref-id (or (indexable-static-info #'#%index-get)
                                    (if more-static?
                                        (raise-syntax-error who (not-static) indexable-in)
                                        #'indexable-get)))
       (define e (datum->syntax (quote-syntax here)
                                (list indexable-ref-id indexable index)
                                (span-srcloc indexable #'head)
                                #'head))
       (define result-static-infos (cond
                                     [(or (indexable-static-info #'#%index-result)
                                          (syntax-local-static-info indexable-ref-id #'#%call-result))
                                      => (lambda (results)
                                           (find-call-result-at results 2 null #f
                                                                (lambda ()
                                                                  (annotation-dependencies
                                                                   (list (indexable-static-infos)
                                                                         (index-static-infos))
                                                                   (hashalw)
                                                                   #f
                                                                   #f))))]
                                     [else #'()]))
       (values e result-static-infos))
     (cond
       [repetition?
        (syntax-parse #'index
          [rep::repetition
           #:with indexable-info::repetition-info indexable
           (values
            (build-compound-repetition #'head (list indexable #'rep.parsed)
                                       #:element-statinfo? #t
                                       (lambda (indexable index)
                                         (build-ref indexable
                                                    index
                                                    (lambda (key)
                                                      (repetition-static-info-lookup #'indexable-info.element-static-infos key))
                                                    (lambda ()
                                                      (repetition-extract-static-infos #'indexable-info.element-static-infos))
                                                    (lambda ()
                                                      (extract-static-infos index)))))
            #'tail)])]
       [else
        (define index-e (rhombus-local-expand #'(rhombus-expression index)))
        (define-values (e result-static-infos)
          (build-ref indexable
                     index-e
                     (lambda (key)
                       (syntax-local-static-info indexable key))
                     (lambda ()
                       (extract-static-infos indexable))
                     (lambda ()
                       (extract-static-infos index-e))))
        (define reloc-e (relocate (respan #`(#,indexable-in args)) e))
        (values (wrap-static-info* reloc-e result-static-infos)
                #'tail)])]
    [(_ (~and args (head::brackets)) . tail)
     (raise-syntax-error who "missing index expression" #'args)]
    [(_ (~and args (head::brackets _ ...)) . tail)
     (raise-syntax-error who "expected a single index expression, found multiple" #'args)]))

(define indexable-get-who 'Indexable.get)
(define indexable-set!-who 'MutableIndexable.set)

(define (indexable-get indexable index)
  (cond
    [(treelist? indexable) (treelist-ref indexable index)]
    [(list? indexable) (list-ref indexable index)]
    [(vector? indexable) (vector-ref indexable index)]
    [(hash? indexable) (hash-ref indexable index)]
    [(string? indexable) (string-ref indexable index)]
    [(bytes? indexable) (bytes-ref indexable index)]
    [(mutable-treelist? indexable) (mutable-treelist-ref indexable index)]
    [else
     (define ref (indexable-ref indexable #f))
     (unless ref
       (raise-annotation-failure indexable-get-who indexable "Indexable"))
     (ref indexable index)]))

(define (indexable-set! indexable index val)
  (cond
    [(mutable-vector? indexable) (vector-set! indexable index val)]
    [(mutable-hash? indexable) (hash-set! indexable index val)]
    [(mutable-bytes? indexable) (bytes-set! indexable index val)]
    [(mutable-treelist? indexable) (mutable-treelist-set! indexable index val)]
    [else
     (define set (setable-ref indexable #f))
     (unless set
       (raise-annotation-failure indexable-set!-who indexable "MutableIndexable"))
     (set indexable index val)]))
