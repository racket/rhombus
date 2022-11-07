#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/stxparam-exptime
                     enforest/syntax-local
                     "class-parse.rkt"
                     "tag.rkt"
                     "srcloc.rkt")
         racket/stxparam
         "expression.rkt"
         "parse.rkt"
         "entry-point.rkt"
         "function-arity-key.rkt"
         "dot-provider-key.rkt"
         "static-info.rkt"
         (submod "dot.rkt" for-dot-provider)
         "assign.rkt"
         "parens.rkt"
         (submod "function.rkt" for-call))

(provide (for-syntax build-method-map
                     build-methods

                     get-private-table)

         this
         super

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
                      (values i (let ([m (vector-ref vtable i)])
                                  (if (eq? (syntax-e m) '#:unimplemented)
                                      '#:unimplemented
                                      m)))))
  (define-values (new-ht new-vtable-ht priv-ht here-ht)
    (for/fold ([ht ht] [vtable-ht vtable-ht] [priv-ht #hasheq()] [here-ht #hasheq()]) ([added (in-list added-methods)])
      (define id (added-method-id added))
      (define new-here-ht (hash-set here-ht (syntax-e id) id))
      (cond
        [(hash-ref here-ht (syntax-e id) #f)
         (raise-syntax-error #f "duplicate method name" stx id)]
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
                         (hash-set vtable-ht (unbox pos) (added-method-rhs-id added))
                         priv-ht
                         new-here-ht)]
                [else
                 (raise-syntax-error #f "method is already in superclass" stx id)]))]
        [else
         (cond
           [(memq (added-method-mode added) '(override final-override))
            (raise-syntax-error #f "method is not in superclass" stx id)]
           [(eq? (added-method-mode added) 'private)
            (values ht
                    vtable-ht
                    (hash-set priv-ht (syntax-e id) (added-method-rhs-id added))
                    new-here-ht)]
           [else
            (define pos (hash-count vtable-ht))
            (values (hash-set ht (syntax-e id)
                              (cons (if (eq? (added-method-mode added) 'final)
                                        pos
                                        (box pos))
                                    id))
                    (hash-set vtable-ht pos (added-method-rhs-id added))
                    priv-ht
                    new-here-ht)])])))
  (values (for/hasheq ([(k pos+id) (in-hash new-ht)])
            (values k (car pos+id)))
          (for/hasheqv ([(s pos+id) (in-hash new-ht)])
            (define i (car pos+id))
            (values (if (box? i) (unbox i) i) (cdr pos+id)))
          (for/vector ([i (in-range (hash-count new-vtable-ht))])
            (hash-ref new-vtable-ht i))
          priv-ht
          here-ht
          (for/or ([v (in-hash-values new-vtable-ht)])
            (eq? v '#:unimplemented))))

(define-syntax-parameter this-id #f)
(define-syntax-parameter private-tables #f)

(define-syntax this
  (expression-transformer
   #'this
   (lambda (stxs)
     (syntax-parse stxs
       [(head . tail)
        (cond
          [(syntax-parameter-value #'this-id)
           => (lambda (id+dp+super)
                (syntax-parse id+dp+super
                  [(id dp . super-id)
                   (values (wrap-static-info (datum->syntax #'id (syntax-e #'id) #'head #'head)
                                             #'#%dot-provider
                                             #'dp)
                           #'tail)]))]
          [else
           (raise-syntax-error #f
                               "allowed only within methods"
                               #'head)])]))))

(define-syntax super
  (expression-transformer
   #'this
   (lambda (stxs)
     (syntax-parse stxs
       #:datum-literals (op |.|)
       [(head (op |.|) method-id:identifier (~and args (tag::parens arg ...)) . tail)
        (cond
          [(syntax-parameter-value #'this-id)
           => (lambda (id+dp+super)
                (syntax-parse id+dp+super
                  [(id dp . super-id)
                   (unless (syntax-e #'super-id)
                     (raise-syntax-error #f "class has no superclass" #'head))
                   (define super (syntax-local-value* (in-class-desc-space #'super-id) class-desc-ref))
                   (unless super
                     (raise-syntax-error #f "class not found" #'super-id))
                   (define pos (hash-ref (class-desc-method-map super) (syntax-e #'method-id) #f))
                   (unless pos
                     (raise-syntax-error #f "no such method in superclass" #'head #'method-id))
                   (define impl (vector-ref (syntax-e (class-desc-method-vtable super)) (if (box? pos) (unbox pos) pos)))
                   (when (eq? (syntax-e impl) '#:unimplemented)
                     (raise-syntax-error #f "method is unimplemented in superclass" #'head #'method-id))
                   (define-values (call new-tail)
                     (parse-function-call impl (list #'id) #'(method-id args)))
                   (values call #'tail)]))]
          [else
           (raise-syntax-error #f
                               "allowed only within methods"
                               #'head)])]))))

(define-for-syntax (get-private-tables)
  (let ([id (syntax-parameter-value #'private-tables)])
    (if id
        (syntax-local-value id)
        '())))

(define-for-syntax (get-private-table desc)
  (define tables (get-private-tables))
  (or (for/or ([t (in-list tables)])
        (and (free-identifier=? (car t) (class-desc-id desc))
             (cdr t)))
      #hasheq()))

(define-for-syntax (make-field-syntax id accessor-id maybe-mutator-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       #:literals (:=)
       [(head (op :=) rhs ...)
        #:when (syntax-e maybe-mutator-id)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(obj-id . _)
           (values (no-srcloc
                    #`(let ([#,id (rhombus-expression (#,group-tag rhs ...))])
                        #,(datum->syntax #'here
                                         (list maybe-mutator-id #'obj-id id)
                                         #'head
                                         #'head)
                        #,id))
                   #'())])]
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
                       [method-index/id ...]
                       [private-method-name ...]
                       [private-method-id ...]
                       [private-field-name ...]
                       [private-field-desc ...]
                       super-name)
                 names])
    (with-syntax ([(field-name ...) (for/list ([id (in-list (syntax->list #'(field-name ...)))])
                                      (datum->syntax #'name (syntax-e id) id id))])
      (list
       #`(define-values #,(for/list ([added (in-list added-methods)]
                                     #:when (added-method-rhs added))
                            (added-method-rhs-id added))
           (let ()
             (define-syntax field-name (make-field-syntax (quote-syntax field-name)
                                                          (quote-syntax name-field)
                                                          (quote-syntax maybe-set-name-field!)))
             ...
             (define-syntax method-name (make-method-syntax (quote-syntax method-name)
                                                            (quote-syntax method-index/id)))
             ...
             (define-syntax private-method-name (make-method-syntax (quote-syntax private-method-name)
                                                                    (quote-syntax private-method-id)))
             ...
             (define-syntax new-private-tables (cons (cons (quote-syntax name)
                                                           (hasheq (~@ 'private-method-name
                                                                       (quote-syntax private-method-id))
                                                                   ...
                                                                   (~@ 'private-field-name
                                                                       private-field-desc)
                                                                   ...))
                                                     (get-private-tables)))
             (values
              #,@(for/list ([added (in-list added-methods)]
                            #:when (added-method-rhs added))
                   #`(let ([#,(added-method-id added) (method-block #,(added-method-rhs added)
                                                                    name-instance
                                                                    new-private-tables
                                                                    super-name)])
                       #,(added-method-id added))))))))))

(define-syntax (method-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(_ (block expr)
        name-instance
        private-tables-id
        super-name)
     #:with (~var e (:entry-point (entry-point-adjustments
                                   (list #'this-obj)
                                   (lambda (stx)
                                     #`(syntax-parameterize ([this-id (quote-syntax (this-obj name-instance . super-name))]
                                                             [private-tables (quote-syntax private-tables-id)])
                                         #,stx))
                                   #t)))
     #'expr
     #'e.parsed]))
