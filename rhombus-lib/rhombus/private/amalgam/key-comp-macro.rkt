#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse)
         "space-provide.rkt"
         "definition.rkt"
         "mutability.rkt"
         "key-comp.rkt"
         "key-comp-runtime.rkt"
         "parens.rkt"
         "to-list.rkt"
         (submod "map.rkt" for-key-comp-macro)
         (submod "set.rkt" for-key-comp-macro)
         "annotation-failure.rkt"
         "parse.rkt"
         "key-comp-property.rkt"
         "../version-case.rkt")

(module+ for-key-comp
  (provide (for-syntax build-key-comp)))

(define+provide-space key_comp rhombus/key_comp
  #:fields
  (def))

(define-defn-syntax def
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ (_::quotes (group name::name))
            (body-tag::block
             (~and
              (~seq (group kw clause-block) ...)
              (~seq
               (~alt (~optional (group #:equals
                                       (equals-tag::block
                                        equals-body ...)))
                     (~optional (group #:hash_code
                                       (hash-code-tag::block
                                        hash-code-body ...))))
               ...))))
         (unless (attribute equals-tag)
           (raise-syntax-error #f "missing an `~equals` clause" stx))
         (unless (attribute hash-code-tag)
           (raise-syntax-error #f "missing a `~hash_code` clause" stx))
         #`((define-values (x-equals? x-hash-code)
              (hash-procedures (~@ kw (rhombus-body-expression clause-block)) ...))
            #,@(build-key-comp-runtime #'name.name #'x-equals? #'x-hash-code #'x-map? #'wrap)
            #,@(build-key-comp #'name.name #'x-map? #'wrap))]))))

(define-for-syntax (build-key-comp name-id x-map?-id wrap-id)
  (with-syntax ([name name-id]
                [x-map? x-map?-id]
                [wrap wrap-id])
    (with-syntax ([x-map-pair-build (datum->syntax #'here (string->symbol
                                                           (format "Map.by(~s)" (syntax-e #'name))))]
                  [x-mutable-map-build (datum->syntax #'here (string->symbol
                                                              (format "MutableMap.by(~s)" (syntax-e #'name))))]
                  [x-weak-mutable-map-build (datum->syntax #'here (string->symbol
                                                                   (format "WeakMutableMap.by(~s)" (syntax-e #'name))))]
                  [x-set-build (datum->syntax #'here (string->symbol
                                                      (format "Set.by(~s)" (syntax-e #'name))))]
                  [x-mutable-set-build (datum->syntax #'here (string->symbol
                                                              (format "MutableSet.by(~s)" (syntax-e #'name))))]
                  [x-weak-mutable-set-build (datum->syntax #'here (string->symbol
                                                                   (format "WeakMutableSet.by(~s)" (syntax-e #'name))))])
      #`(;; See "key-comp-runtime.rkt" for wrapper content
         (define empty-x-map (wrap #hash()))
         (define (x-map-build . args)
           (build-map 'x-map-build empty-x-map (args->pairs 'x-map-build args)))
         (define (x-map-pair-build . pairs)
           (build-map 'x-map-build empty-x-map (to-list 'x-map-build pairs)))
         (define-syntax for/x-map (make-map-for #'list->x-map))
         (define (list->x-map pairs)
           (build-map 'list->x-map empty-x-map (to-pairs (to-list 'list->x-map pairs))))
         (define (mutable-x-map? v)
           (and (mutable-hash? v) (x-map? v)))
         (define (x-mutable-map-build args)
           (define ht (wrap (make-hash)))
           (build-mutable-map 'x-mutable-map-build ht (to-list 'x-mutable-map-build args)))
         (define (weak-mutable-x-map? v)
           (and (mutable-hash? v) (hash-ephemeron? v) (x-map? v)))
         (define (x-weak-mutable-map-build args)
           (define ht (wrap (make-ephemeron-hash)))
           (build-mutable-map 'x-weak-mutable-map-build ht (to-list 'x-weak-mutable-map-build args)))
         (define (immutable-x-set? v)
           (and (set? v) (immutable-hash? (set-ht v)) (x-map? (set-ht v))))
         (define (x-set-build . args)
           (list->x-set args))
         (define-syntax for/x-set (make-set-for #'list->x-set))
         (define (list->x-set args)
           (x-map-set-build args empty-x-map))
         (define (mutable-x-set? v)
           (and (set? v) (mutable-hash? (set-ht v)) (x-map? (set-ht v))))
         (define (x-mutable-set-build . args)
           (define ht (wrap (make-hash)))
           (build-mutable-set 'x-mutable-set-build ht args))
         (define (weak-mutable-x-set? v)
           (and (set? v) (mutable-hash? (set-ht v)) (hash-weak? (set-ht v)) (x-map? (set-ht v))))
         (define (x-weak-mutable-set-build . args)
           (define ht (wrap (make-weak-hash)))
           (build-mutable-set 'x-weak-mutable-set-build ht args))
         (define-key-comp-syntax name
           (key-comp-maker
            (lambda ()
              (key-comp 'name #'x-map?
                        #'x-map-build #'x-map-pair-build #'for/x-map
                        #'mutable-x-map? #'x-mutable-map-build
                        #'weak-mutable-x-map? #'x-weak-mutable-map-build
                        #'empty-x-map
                        #'immutable-x-set?
                        #'x-set-build #'x-set-build #'for/x-set
                        #'mutable-x-set? #'x-mutable-set-build
                        #'weak-mutable-x-set? #'x-weak-mutable-set-build))))))))

(define (hash-procedures #:equals equals #:hash_code hash_code)
  (unless (and (procedure? equals) (procedure-arity-includes? equals 3))
    (raise-annotation-failure 'key_comp.def equals "Function.of_arity(3)"))
  (unless (and (procedure? hash_code) (procedure-arity-includes? hash_code 2))
    (raise-annotation-failure 'key_comp.def hash_code "Function.of_arity(2)"))
  (values equals hash_code))

(define (x-map-set-build elems ht)
  (set
   (for/fold ([ht ht]) ([e (in-list elems)])
     (hash-set ht e #t))))

(define (build-mutable-map who ht args)
  (for ([p (in-list (args->pairs who args))])
    (hash-set! ht (car p) (cadr p)))
  ht)

(define (build-mutable-set who ht args)
  (for ([e (in-list args)])
    (hash-set! ht e #t))
  (set ht))

(define (args->pairs who orig-args)
  (let loop ([args orig-args])
    (cond
      [(null? args) null]
      [(null? (cdr args))
       (raise-arguments-error who "expected an even number of arguments")]
      [else (cons (list (car args) (cadr args)) (loop (cddr args)))])))

(define (to-pairs l)
  (for/list ([p (in-list l)])
    (list (car p) (cdr p))))

(define-for-syntax (make-map-for list->x)
  (lambda (stx)
    (syntax-parse stx
      [(_ clauses body) #`(#,list->x (for/list clauses (let-values ([(a d) body]) (cons a d))))])))

(define-for-syntax (make-set-for list->x)
  (lambda (stx)
    (syntax-parse stx
      [(_ clauses body) #`(#,list->x (for/list clauses body))])))
