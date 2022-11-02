#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/stxparam-exptime
                     "class-parse.rkt")
         racket/stxparam
         "expression.rkt"
         "parse.rkt"
         "entry-point.rkt"
         "function-arity-key.rkt"
         "dot-provider-key.rkt"
         "static-info.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parens.rkt"
         (submod "function.rkt" for-call))

(provide (for-syntax build-method-map
                     build-methods)

         this

         prop:methods
         method-ref
         method-curried-ref)

(define-values (prop:methods prop-methods? prop-methods-ref)
  (make-struct-type-property 'methods))

(define (method-ref obj pos)
  (define vtable (prop-methods-ref obj #f))
  (vector-ref vtable pos))

(define (method-curried-ref obj pos)
  (curry-method (method-ref obj pos) obj))

(define-for-syntax (build-method-map stx added-methods super)
  (define ht (if super
                 (for/hasheq ([name (class-desc-method-names super)]
                              [i (in-naturals)])
                   (if (box? name)
                       (values (unbox name) (cons (box i) (unbox name)))
                       (values name (cons i name))))
                 #hasheq()))
  (define vtable (if super
                     (syntax-e (class-desc-method-vtable super))
                     '#()))
  (define vtable-ht (for/hasheqv ([i (in-range (vector-length vtable))])
                      (values i (vector-ref vtable i))))
  (define-values (new-ht new-vtable-ht)
    (for/fold ([ht ht] [vtable-ht vtable-ht]) ([added (in-list added-methods)])
      (define id (added-method-id added))
      (cond
        [(hash-ref ht (syntax-e id) #f)
         => (lambda (pos+id)
              (define pos (car pos+id))
              (cond
                [(memq (added-method-mode added) '(override final-override))
                 (when (integer? pos)
                   (raise-syntax-error #f "cannot override superclass's final method" stx id))
                 (values (if (eq? (added-method-mode added) 'final-override)
                             (hash-set ht (syntax-e id) (cons (unbox pos) id))
                             ht)
                         (hash-set vtable-ht (unbox pos) (added-method-rhs-id added)))]
                [else
                 (raise-syntax-error #f "method is already in superclass" stx id)]))]
        [else
         (cond
           [(memq (added-method-mode added) '(override final-override))
            (raise-syntax-error #f "method is not in superclass" stx id)]
           [(eq? (added-method-mode added) 'private)
            ;; skip private methods in this pass
            (values ht vtable-ht)]
           [else
            (define pos (hash-count vtable-ht))
            (values (hash-set ht (syntax-e id)
                              (cons (if (eq? (added-method-mode added) 'final)
                                        pos
                                        (box pos))
                                    id))
                    (hash-set vtable-ht pos (added-method-rhs-id added)))])])))
  (values (for/hasheq ([(k pos+id) (in-hash new-ht)])
            (values k (car pos+id)))
          (for/hasheqv ([(s pos+id) (in-hash new-ht)])
            (define i (car pos+id))
            (values (if (box? i) (unbox i) i) (cdr pos+id)))
          (for/vector ([i (in-range (hash-count new-vtable-ht))])
            (hash-ref new-vtable-ht i))))

(define-syntax-parameter this-id #f)

(define-syntax this
  (expression-transformer
   #'this
   (lambda (stxs)
     (syntax-parse stxs
       [(head . tail)
        (cond
          [(syntax-parameter-value #'this-id)
           => (lambda (id+dp)
                (syntax-parse id+dp
                  [(id . dp)
                   (values (wrap-static-info (datum->syntax #'id (syntax-e #'id) #'head #'head)
                                             #'#%dot-provider
                                             #'dp)
                           #'tail)]))]
          [else
           (raise-syntax-error #f
                               "allowed only within methods"
                               #'head)])]))))

(define-for-syntax (make-field-syntax id accessor-id maybe-mutator-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(head . tail)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(id . _)
           (values (datum->syntax #'here
                                  (list accessor-id #'id)
                                  #'head
                                  #'head)
                   #'tail)])]))))

(define-for-syntax (make-method-syntax id index/id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       [(head (~and args (tag::parens arg ...)) . tail)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(id . _)
           (define rator (if (identifier? index/id)
                             index/id
                             #`(vector-ref (prop-methods-ref id) #,index/id)))
           (define-values (call new-tail)
             (parse-function-call rator (list #'id) #'(head args)))
           (values call #'tail)])]
       [(head . _)
        (raise-syntax-error #f
                            "method must be called"
                            #'head)]))))

(define-for-syntax (build-methods added-methods names)
  (with-syntax ([(name name-instance
                       [field-name ...]
                       [name-field ...]
                       [maybe-set-name-field! ...]
                       [method-name ...]
                       [method-index/id ...])
                 names])
    (with-syntax ([(field-name ...) (for/list ([id (in-list (syntax->list #'(field-name ...)))])
                                      (datum->syntax #'name (syntax-e id) id id))])
      (list
       #`(define-values #,(map added-method-rhs-id added-methods)
           (let ()
             (define-syntax field-name (make-field-syntax (quote-syntax field-name)
                                                          (quote-syntax name-field)
                                                          (quote-syntax maybe-set-name-field!)))
             ...
             (define-syntax method-name (make-method-syntax (quote-syntax method-name)
                                                            (quote-syntax method-index/id)))
             ...
             (values
              #,@(for/list ([added (in-list added-methods)])
                   #`(let ([#,(added-method-id added) (method-block #,(added-method-rhs added)
                                                                    name-instance)])
                       #,(added-method-id added))))))))))

(define-syntax (method-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(_ (block expr)
        name-instance)
     #:with (~var e (:entry-point (entry-point-adjustments
                                   (list #'this-obj)
                                   (lambda (stx)
                                     #`(syntax-parameterize ([this-id (quote-syntax (this-obj . name-instance))])
                                         #,stx))
                                   #t)))
     #'expr
     #'e.parsed]))
