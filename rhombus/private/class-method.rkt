#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/stxparam-exptime
                     enforest/syntax-local
                     "class-parse.rkt"
                     "interface-parse.rkt"
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
         (submod "function.rkt" for-call)
         "realm.rkt")

(provide (for-syntax build-method-map
                     build-interface-vtable
                     build-methods

                     get-private-table)

         this
         super

         prop:methods
         prop-methods-ref
         method-ref
         method-curried-ref

         raise-not-an-instance)

(define-values (prop:methods prop-methods? prop-methods-ref)
  (make-struct-type-property 'methods))

(define-syntax (method-ref stx)
  (syntax-parse stx
    [(_ ref obj pos) #`(vector-ref (ref obj) pos)]))

(define (method-curried-ref ref obj pos)
  (curry-method (method-ref ref obj pos) obj))

(define (raise-not-an-instance name v)
  (raise-argument-error* name rhombus-realm "not an instance for method call" v))

(define-for-syntax (build-method-map stx added-methods super interfaces private-interfaces)
  (define supers (if super (cons super interfaces) interfaces))
  (define-values (super-str supers-str)
    (cond
      [(null? interfaces)
       (values "superclass" "superclasses(!?)")]
      [(not super)
       (values "interface" "superinterfaces")]
      [else
       (values "class or interface" "classes or superinterfaces")]))

  ;; create merged method tables from the superclass (if any) and all superinterfaces;
  ;; we start with the superclass, if any, so the methods from its vtable stay
  ;; in the same place in the new vtable
  (define-values (ht            ; symbol -> (cons maybe-boxed-index id)
                  super-priv-ht ; symbol -> identifier, implies not in `ht`
                  vtable-ht)    ; int -> accessor-identifier or '#:unimplemented
    (for/fold ([ht #hasheq()] [priv-ht #hasheq()] [vtable-ht #hasheqv()]) ([super (in-list supers)])
      (define super-vtable (super-method-vtable super))
      (define private? (hash-ref private-interfaces super #f))
      (for/fold ([ht ht] [priv-ht priv-ht] [vtable-ht vtable-ht])
                ([name (super-method-names super)]
                 [super-i (in-naturals)])
        (define i (hash-count ht))
        (define new-rhs (let ([rhs (vector-ref super-vtable super-i)])
                          (if (eq? (syntax-e rhs) '#:unimplemented) '#:unimplemented rhs)))
        (define-values (key val)
          (if (box? name)
              (values (unbox name) (cons (box i) (unbox name)))
              (values name (cons i name))))
        (define old-val (or (hash-ref ht key #f)
                            (hash-ref priv-ht key #f)))
        (cond
          [old-val
           (define old-rhs (if (pair? old-val)
                               (let ([old-i (car old-val)])
                                 (hash-ref vtable-ht (if (box? old-i) (unbox old-i) old-i)))
                               old-val))
           (unless (or (if (identifier? old-rhs)
                           (and (identifier? new-rhs)
                                (free-identifier=? old-rhs new-rhs))
                           (eq? old-rhs new-rhs))
                       (for/or ([added (in-list added-methods)])
                         (and (eq? key (syntax-e (added-method-id added)))
                              (memq (added-method-mode added) '(override final-override private-override))))) 
             (raise-syntax-error #f (format "method supplied by multiple ~a and not overridden" supers-str) stx key))
           (if (eq? (not private?) (pair? old-val))
               (values ht priv-ht vtable-ht)
               (values (hash-set vtable-ht key val)
                       (hash-remove priv-ht key)
                       (hash-set vtable-ht i new-rhs)))]
          [private?
           (values ht
                   (hash-set priv-ht key new-rhs)
                   vtable-ht)]
          [else
           (values (hash-set ht key val)
                   priv-ht
                   (hash-set vtable-ht i new-rhs))]))))

  ;; add methods for the new class/interface
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
                   (raise-syntax-error #f (format "cannot override ~a's final method" super-str) stx id))
                 (values (if (eq? (added-method-mode added) 'final-override)
                             (hash-set ht (syntax-e id) (cons (unbox pos) id))
                             ht)
                         (hash-set vtable-ht (unbox pos) (added-method-rhs-id added))
                         priv-ht
                         new-here-ht)]
                [(memq (added-method-mode added) '(private-override))
                 (raise-syntax-error #f (format "method is not in private ~a" super-str) stx id)]
                [else
                 (raise-syntax-error #f (format "method is already in ~a" super-str) stx id)]))]
        [(hash-ref super-priv-ht (syntax-e id) #f)
         => (lambda (rhs)
              (cond
                [(eq? (added-method-mode added) 'private-override)
                 (values ht
                         vtable-ht
                         (hash-set priv-ht (syntax-e id) (added-method-rhs-id added))
                         new-here-ht)]
                [(memq (added-method-mode added) '(override final-override))
                 (raise-syntax-error #f (format "method is in private ~a" super-str) stx id)]
                [else
                 (raise-syntax-error #f (format "method is already in private ~a" super-str) stx id)]))]
        [else
         (cond
           [(memq (added-method-mode added) '(override final-override))
            (raise-syntax-error #f (format "method is not in ~a" super-str) stx id)]
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

  (for ([(name rhs) (in-hash super-priv-ht)])
    (when (eq? rhs '#:unimplemented)
      (unless (hash-ref priv-ht name #f)
        (raise-syntax-error #f (format "method from private ~a must be overridden" super-str) stx name))))

  (define method-map
    (for/hasheq ([(k pos+id) (in-hash new-ht)])
      (values k (car pos+id))))
  (define method-names
    (for/hasheqv ([(s pos+id) (in-hash new-ht)])
      (define i (car pos+id))
      (values (if (box? i) (unbox i) i) (cdr pos+id))))
  (define method-vtable
    (for/vector ([i (in-range (hash-count new-vtable-ht))])
      (hash-ref new-vtable-ht i)))
  (define unimplemented-name
    (for/or ([v (in-hash-values new-vtable-ht)]
             [i (in-naturals)])
      (and (eq? v '#:unimplemented)
           (hash-ref method-names i))))

  (values method-map
          method-names
          method-vtable
          priv-ht
          here-ht
          unimplemented-name))

(define-for-syntax (build-interface-vtable intf method-map method-vtable method-names method-private)
  (for/list ([maybe-boxed-name (in-vector (interface-desc-method-names intf))])
    (define name (if (box? maybe-boxed-name) (unbox maybe-boxed-name) maybe-boxed-name))
    (cond
      [(hash-ref method-private name #f)
       => (lambda (id) id)]
      [else
       (define maybe-boxed-pos (hash-ref method-map name))
       (define pos (if (box? maybe-boxed-pos) (unbox maybe-boxed-pos) maybe-boxed-pos))
       (vector-ref method-vtable pos)])))

(define-for-syntax (super-method-vtable p)
  (syntax-e
   (if (class-desc? p)
       (class-desc-method-vtable p)
       (interface-desc-method-vtable p))))

(define-for-syntax (super-method-names p)
  (if (class-desc? p)
      (class-desc-method-names p)
      (interface-desc-method-names p)))

(define-for-syntax (super-method-map p)
  (if (class-desc? p)
      (class-desc-method-map p)
      (interface-desc-method-map p)))

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
           => (lambda (id+dp+supers)
                (syntax-parse id+dp+supers
                  [(id dp . _)
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
           => (lambda (id+dp+supers)
                (syntax-parse id+dp+supers
                  [(id dp)
                   (raise-syntax-error #f "class has no superclass" #'head)]
                  [(id dp . super-ids)
                   (define super+pos
                     (for/or ([super-id (in-list (syntax->list #'super-ids))])
                       (define super (syntax-local-value* (in-class-desc-space super-id)
                                                          (lambda (v)
                                                            (or (class-desc-ref v)
                                                                (interface-desc-ref v)))))
                       (unless super
                         (raise-syntax-error #f "class or interface not found" super-id))
                       (define pos (hash-ref (super-method-map super) (syntax-e #'method-id) #f))
                       (and pos (cons super pos))))
                   (unless super+pos
                     (raise-syntax-error #f "no such method in superclass" #'head #'method-id))
                   (define super (car super+pos))
                   (define pos (cdr super+pos))
                   (define impl (vector-ref (super-method-vtable super) (if (box? pos) (unbox pos) pos)))
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

(define-for-syntax (build-methods added-methods method-map method-names method-private
                                  names)
  (with-syntax ([(name name-instance name?
                       [field-name ...]
                       [name-field ...]
                       [maybe-set-name-field! ...]
                       [private-field-name ...]
                       [private-field-desc ...]
                       [super-name ...])
                 names])
    (with-syntax ([(field-name ...) (for/list ([id (in-list (syntax->list #'(field-name ...)))])
                                      (datum->syntax #'name (syntax-e id) id id))]
                  [((method-name method-index/id) ...) (for/list ([i (in-range (hash-count method-map))])
                                                         (define m-name (let ([n (hash-ref method-names i)])
                                                                          (if (syntax? n)
                                                                              (syntax-e n)
                                                                              n)))
                                                         (define id/boxed (hash-ref method-map m-name))
                                                         (list (datum->syntax #'name m-name)
                                                               (if (box? id/boxed)
                                                                   i
                                                                   id/boxed)))]
                  [((private-method-name private-method-id) ...) (for/list ([m-name (in-list (sort (hash-keys method-private)
                                                                                                   symbol<?))])
                                                                   (list (datum->syntax #'name m-name)
                                                                         (hash-ref method-private m-name)))])
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
                                                                    name name-instance name?
                                                                    new-private-tables
                                                                    [super-name ...])])
                       #,(added-method-id added))))))))))

(define-syntax (method-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(_ (block expr)
        name name-instance name?
        private-tables-id
        super-names)
     #:with (~var e (:entry-point (entry-point-adjustments
                                   (list #'this-obj)
                                   (lambda (stx)
                                     #`(syntax-parameterize ([this-id (quote-syntax (this-obj name-instance . super-names))]
                                                             [private-tables (quote-syntax private-tables-id)])
                                         ;; This check might be redundant, depending on how the method was called:
                                         (unless (name? this-obj) (raise-not-an-instance 'name this-obj))
                                         (let ()
                                           #,stx)))
                                   #t)))
     #'expr
     #'e.parsed]))
