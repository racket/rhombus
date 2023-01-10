#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/stxparam-exptime
                     enforest/syntax-local
                     "class-parse.rkt"
                     "interface-parse.rkt"
                     "tag.rkt"
                     "srcloc.rkt"
                     "statically-str.rkt")
         racket/stxparam
         "expression.rkt"
         "parse.rkt"
         "entry-point.rkt"
         "class-this.rkt"
         "class-method-result.rkt"
         "dot-provider-key.rkt"
         "static-info.rkt"
         (submod "dot.rkt" for-dot-provider)
         "assign.rkt"
         "parens.rkt"
         (submod "function.rkt" for-call)
         (only-in (submod "implicit.rkt" for-dynamic-static)
                  static-#%call)
         "realm.rkt")

(provide (for-syntax extract-method-tables
                     build-interface-vtable
                     build-quoted-method-map
                     build-quoted-method-shapes
                     build-quoted-private-method-list
                     build-method-results
                     build-method-result-expression
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
  (raise-arguments-error* name rhombus-realm "not an instance for method call" "value" v))

;; Results:
;;   method-mindex   ; symbol -> mindex
;;   method-names    ; index -> symbol-or-identifier; symbol is inherited
;;   method-vtable   ; index -> function-identifier or '#:abstract
;;   method-results  ; symbol -> nonempty list of identifiers; first one implies others
;;   method-private  ; symbol -> identifier or (list identifier); list means property; non-super symbol's identifier attached as 'lhs-id
;;   method-decls    ; symbol -> identifier, intended for checking distinct
;;   abstract-name   ; #f or identifier for a still-abstract method

(define-for-syntax (extract-method-tables stx added-methods super interfaces private-interfaces final?)
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
  (define-values (ht            ; symbol -> (cons mindex id)
                  super-priv-ht ; symbol -> identifier or (list identifier), implies not in `ht`
                  vtable-ht)    ; int -> accessor-identifier or '#:abstract
    (for/fold ([ht #hasheq()] [priv-ht #hasheq()] [vtable-ht #hasheqv()]) ([super (in-list supers)])
      (define super-vtable (super-method-vtable super))
      (define private? (hash-ref private-interfaces super #f))
      (for/fold ([ht ht] [priv-ht priv-ht] [vtable-ht vtable-ht])
                ([shape (super-method-shapes super)]
                 [super-i (in-naturals)])
        (define i (hash-count ht))
        (define new-rhs (let ([rhs (vector-ref super-vtable super-i)])
                          (if (eq? (syntax-e rhs) '#:abstract) '#:abstract rhs)))
        (define-values (key val)
          (let* ([arity (and (vector? shape) (vector-ref shape 1))]
                 [shape (if (vector? shape) (vector-ref shape 0) shape)]
                 [property? (pair? shape)]
                 [shape (if (pair? shape) (car shape) shape)]
                 [final? (not (box? shape))]
                 [shape (if (box? shape) (unbox shape) shape)])
            (values shape (cons (mindex i final? property? arity) shape))))
        (define old-val (or (hash-ref ht key #f)
                            (hash-ref priv-ht key #f)))
        (cond
          [old-val
           (define old-rhs (cond
                             [(and (pair? old-val)
                                   (mindex? (car old-val)))
                              (let ([old-i (car old-val)])
                                (hash-ref vtable-ht (mindex-index old-i)))]
                             [(pair? old-val) (car old-val)]
                             [else old-val]))
           (unless (or (if (identifier? old-rhs)
                           (and (identifier? new-rhs)
                                (free-identifier=? old-rhs new-rhs))
                           (eq? old-rhs new-rhs))
                       (for/or ([added (in-list added-methods)])
                         (and (eq? key (syntax-e (added-method-id added)))
                              (eq? 'override (added-method-replace added)))))
             (raise-syntax-error #f (format "method supplied by multiple ~a and not overridden" supers-str) stx key))
           (if (or private?
                   (and (pair? old-val) (mindex? (car old-val))))
               (values ht priv-ht vtable-ht)
               (values (hash-set vtable-ht key val)
                       (hash-remove priv-ht key)
                       (hash-set vtable-ht i new-rhs)))]
          [private?
           (define (property-shape? shape) (or (pair? shape) (and (vector? shape) (pair? (vector-ref shape 0)))))
           (values ht
                   (hash-set priv-ht key (if (property-shape? shape)
                                             (list new-rhs)
                                             new-rhs))
                   vtable-ht)]
          [else
           (values (hash-set ht key val)
                   priv-ht
                   (hash-set vtable-ht i new-rhs))]))))

  ;; merge method-result tables from superclass and superinterfaces,
  ;; assuming that the names all turn out to be sufficiently distinct
  (define super-method-results
    (for/fold ([method-results (if super
                                   (for/hasheq ([(sym id) (in-hash (class-desc-method-result super))])
                                     (values sym (list id)))
                                   #hasheq())])
              ([intf (in-list interfaces)])
      (for/fold ([method-results method-results]) ([(sym id) (in-hash (interface-desc-method-result intf))])
        (hash-set method-results sym (cons id (hash-ref method-results sym '()))))))

  (define (private-id/property lhs-id added)
    (let ([id (syntax-property (added-method-rhs-id added)
                               'lhs-id
                               lhs-id)])
      (if (eq? (added-method-kind added) 'property)
          (list id)
          id)))

  ;; add methods for the new class/interface
  (define-values (new-ht new-vtable-ht priv-ht here-ht)
    (for/fold ([ht ht] [vtable-ht vtable-ht] [priv-ht #hasheq()] [here-ht #hasheq()]) ([added (in-list added-methods)])
      (define id (added-method-id added))
      (define new-here-ht (hash-set here-ht (syntax-e id) id))
      (define (check-consistent-property property?)
        (if property?
            (when (eq? (added-method-kind added) 'method)
              (raise-syntax-error #f (format "cannot override ~a's property with a non-property method" super-str)
                                  stx id))
            (when (eq? (added-method-kind added) 'property)
              (raise-syntax-error #f (format "cannot override ~a's non-property method with a property" super-str)
                                  stx id))))
      (cond
        [(hash-ref here-ht (syntax-e id) #f)
         (raise-syntax-error #f "duplicate method name" stx id)]
        [(hash-ref ht (syntax-e id) #f)
         => (lambda (mix+id)
              (define mix (car mix+id))
              (cond
                [(eq? 'override (added-method-replace added))
                 (when (eq? (added-method-disposition added) 'private)
                   (raise-syntax-error #f (format "method is not in private ~a" super-str) stx id))
                 (when (mindex-final? mix)
                   (raise-syntax-error #f (format "cannot override ~a's final method" super-str) stx id))
                 (check-consistent-property (mindex-property? mix))
                 (define idx (mindex-index mix))
                 (define final? (eq? (added-method-disposition added) 'final))
                 (values (if (or final?
                                 (not (equal? (added-method-arity added) (mindex-arity mix))))
                             (let ([property? (eq? (added-method-kind added) 'property)]
                                   [arity (added-method-arity added)])
                               (hash-set ht (syntax-e id) (cons (mindex idx final? property? arity) id)))
                             ht)
                         (hash-set vtable-ht idx (added-method-rhs-id added))
                         priv-ht
                         new-here-ht)]
                [else
                 (raise-syntax-error #f (format "method is already in ~a" super-str) stx id)]))]
        [(hash-ref super-priv-ht (syntax-e id) #f)
         => (lambda (rhs)
              (cond
                [(and (eq? (added-method-replace added) 'override)
                      (eq? (added-method-disposition added) 'private))
                 (check-consistent-property (list? rhs))
                 (values ht
                         vtable-ht
                         (hash-set priv-ht (syntax-e id) (private-id/property id added))
                         new-here-ht)]
                [(eq? (added-method-replace added) 'override)
                 (raise-syntax-error #f (format "method is in private ~a" super-str) stx id)]
                [else
                 (raise-syntax-error #f (format "method is already in private ~a" super-str) stx id)]))]
        [else
         (cond
           [(eq? (added-method-replace added) 'override)
            (raise-syntax-error #f (format "method is not in ~a" super-str) stx id)]
           [(eq? (added-method-disposition added) 'private)
            (values ht
                    vtable-ht
                    (hash-set priv-ht (syntax-e id) (private-id/property id added))
                    new-here-ht)]
           [else
            (define pos (hash-count vtable-ht))
            (values (hash-set ht (syntax-e id)
                              (cons (mindex pos
                                            (or final?
                                                (eq? (added-method-disposition added) 'final))
                                            (eq? (added-method-kind added) 'property)
                                            (added-method-arity added))
                                    id))
                    (hash-set vtable-ht pos (added-method-rhs-id added))
                    priv-ht
                    new-here-ht)])])))

  (for ([(name rhs) (in-hash super-priv-ht)])
    (when (eq? rhs '#:abstract)
      (unless (hash-ref priv-ht name #f)
        (raise-syntax-error #f (format "method from private ~a must be overridden" super-str) stx name))))

  (define method-mindex
    (for/hasheq ([(k mix+id) (in-hash new-ht)])
      (values k (car mix+id))))
  (define method-names
    (for/hasheqv ([(s mix+id) (in-hash new-ht)])
      (values (mindex-index (car mix+id)) (cdr mix+id))))
  (define method-vtable
    (for/vector ([i (in-range (hash-count new-vtable-ht))])
      (hash-ref new-vtable-ht i)))
  (define method-results
    (for/fold ([method-results super-method-results]) ([added (in-list added-methods)]
                                                       #:when (added-method-result-id added))
      (define sym (syntax-e (added-method-id added)))
      (hash-set method-results sym (cons (added-method-result-id added)
                                         (hash-ref method-results sym '())))))
  (define method-private
    (for/fold ([ht super-priv-ht]) ([(k v) (in-hash priv-ht)])
      (hash-set ht k v)))
  
  (define abstract-name
    (for/or ([v (in-hash-values new-vtable-ht)]
             [i (in-naturals)])
      (and (eq? v '#:abstract)
           (hash-ref method-names i))))

  (values method-mindex
          method-names
          method-vtable
          method-results
          method-private
          here-ht
          abstract-name))

(define-for-syntax (build-interface-vtable intf method-mindex method-vtable method-names method-private)
  (for/list ([shape (in-vector (interface-desc-method-shapes intf))])
    (define name (let* ([shape (if (vector? shape) (vector-ref shape 0) shape)]
                        [shape (if (pair? shape) (car shape) shape)]
                        [shape (if (box? shape) (unbox shape) shape)])
                   shape))
    (cond
      [(hash-ref method-private name #f)
       => (lambda (id) (if (pair? id) (car id) id))]
      [else
       (define pos (mindex-index (hash-ref method-mindex name)))
       (vector-ref method-vtable pos)])))

(define-for-syntax (build-quoted-method-map method-mindex)
  (for/hasheq ([(sym mix) (in-hash method-mindex)])
    (values sym (mindex-index mix))))

(define-for-syntax (build-quoted-method-shapes method-vtable method-names method-mindex)
  (for/vector ([i (in-range (vector-length method-vtable))])
    (define name (hash-ref method-names i))
    (define mix (hash-ref method-mindex (if (syntax? name) (syntax-e name) name)))
    (define sym ((if (mindex-property? mix) list values)
                 ((if (mindex-final? mix) values box)
                  name)))
    (if (mindex-arity mix)
        (vector sym (mindex-arity mix))
        sym)))

(define-for-syntax (build-quoted-private-method-list mode method-private)
  (sort (for/list ([(sym v) (in-hash method-private)]
                   #:when (eq? mode (if (pair? v) 'property 'method)))
          sym)
        symbol<?))

(define-for-syntax (build-method-results added-methods
                                         method-mindex method-vtable method-private
                                         method-results
                                         in-final?)
  (for/list ([added (in-list added-methods)]
             #:when (added-method-result-id added))
    #`(define-method-result-syntax #,(added-method-result-id added)
        #,(added-method-maybe-ret added)
        #,(cdr (hash-ref method-results (syntax-e (added-method-id added)) '(none)))
        ;; When calls do not go through vtable, also add static info
        ;; as #%call-result to binding; non-vtable calls include final methods
        ;; and `super` calls to non-final methods:
        #,(or (let ([id/property (hash-ref method-private (syntax-e (added-method-id added)) #f)])
                (if (pair? id/property) (car id/property) id/property))
              (and (not (eq? (added-method-body added) 'abstract))
                   (let ([mix (hash-ref method-mindex (syntax-e (added-method-id added)) #f)])
                     (and (or (mindex-final? mix)
                              (not in-final?))
                          (vector-ref method-vtable (mindex-index mix))))))
        #,(added-method-kind added)
        #,(added-method-arity added))))
          
(define-for-syntax (build-method-result-expression method-result)
  #`(hasheq
     #,@(apply append
               (for/list ([(sym ids) (in-hash method-result)])
                 (list #`(quote #,sym)
                       #`(quote-syntax #,(car ids)))))))

(define-for-syntax (super-method-vtable p)
  (syntax-e
   (if (class-desc? p)
       (class-desc-method-vtable p)
       (interface-desc-method-vtable p))))

(define-for-syntax (super-method-shapes p)
  (if (class-desc? p)
      (class-desc-method-shapes p)
      (interface-desc-method-shapes p)))

(define-for-syntax (super-method-map p)
  (if (class-desc? p)
      (class-desc-method-map p)
      (interface-desc-method-map p)))

(define-syntax this
  (expression-transformer
   #'this
   (lambda (stxs)
     (syntax-parse stxs
       [(head . tail)
        (cond
          [(let ([v (syntax-parameter-value #'this-id)])
             (and (not (identifier? v)) v))
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
     (define c-or-id+dp+supers (syntax-parameter-value #'this-id))
     (cond
       [(not c-or-id+dp+supers)
        (raise-syntax-error #f
                            "allowed only within methods and constructors"
                            #'head)]
       [(keyword? (syntax-e (car (syntax-e c-or-id+dp+supers))))
        ;; in a constructor
        (syntax-parse c-or-id+dp+supers
          [(_ make-name)
           (syntax-parse stxs
             [(head . tail)
              (values #'make-name #'tail)])])]
       [else
        ;; in a method
        (define id+dp+supers c-or-id+dp+supers)
        (syntax-parse id+dp+supers
          [(id dp)
           (raise-syntax-error #f "class has no superclass" #'head)]
          [(id dp . super-ids)
           (syntax-parse stxs
             #:datum-literals (op |.|)
             [(head (op (~and dot-op |.|)) method-id:identifier . tail)
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
              (define impl (vector-ref (super-method-vtable super) pos))
              (when (eq? (syntax-e impl) '#:abstract)
                (raise-syntax-error #f "method is abstract in superclass" #'head #'method-id))
              (define shape+arity (vector-ref (super-method-shapes super) pos))
              (define shape (if (vector? shape+arity) (vector-ref shape+arity 0) shape+arity))
              (define shape-arity (and (vector? shape+arity) (vector-ref shape+arity 1)))
              (define static? (free-identifier=? (datum->syntax #'dot-op '#%call)
                                                 #'static-#%call))
              (cond
                [(pair? shape)
                 ;; a property
                 (syntax-parse #'tail
                   #:datum-literals (op)
                   #:literals (:=)
                   [((op :=) . rhs)
                    #:with e::infix-op+expression+tail #'(:= . rhs)
                    (define-values (call new-tail)
                      (parse-function-call impl (list #'id #'e.parsed) #'(method-id (parens))
                                           #:static? static?
                                           #:rator-stx #'head
                                           #:rator-kind 'property
                                           #:rator-arity shape-arity))
                    (values call
                            #'e.tail)]
                   [_
                    (define-values (call new-tail)
                      (parse-function-call impl (list #'id) #'(method-id (parens))
                                           #:static? static?
                                           #:rator-stx #'head
                                           #:rator-kind 'property
                                           #:rator-arity shape-arity))
                    (values call
                            #'tail)])]
                [else
                 ;; a method
                 (syntax-parse #'tail
                   [((~and args (tag::parens arg ...)) . tail)
                    (define-values (call new-tail)
                      (parse-function-call impl (list #'id) #'(method-id args)
                                           #:static? static?
                                           #:rator-stx #'head
                                           #:rator-kind 'method
                                           #:rator-arity shape-arity))
                    (values call #'tail)])])])])]))))

(define-for-syntax (get-private-table desc)
  (define tables (get-private-tables))
  (or (for/or ([t (in-list tables)])
        (and (free-identifier=? (car t) (class-desc-id desc))
             (cdr t)))
      #hasheq()))

(define-for-syntax (make-field-syntax id static-infos accessor-id maybe-mutator-id)
  (expression-transformer
   id
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op)
       #:literals (:=)
       [(head (op :=) . tail)
        #:when (syntax-e maybe-mutator-id)
        #:with e::infix-op+expression+tail #'(:= . tail)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(obj-id . _)
           (values (no-srcloc
                    #`(let ([#,id e.parsed])
                        #,(datum->syntax #'here
                                         (list maybe-mutator-id #'obj-id id)
                                         #'head
                                         #'head)))
                   #'e.tail)])]
       [(head . tail)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(id . _)
           (values (wrap-static-info* (datum->syntax #'here
                                                     (list accessor-id #'id)
                                                     #'head
                                                     #'head)
                                      static-infos)
                   #'tail)])]))))

(define-for-syntax (make-method-syntax id index/id result-id kind)
  (define (add-method-result call r)
    (if r
        (wrap-static-info* call (method-result-static-infos r))
        call))
  (cond
    [(eq? kind 'property)
     (expression-transformer
      id
      (lambda (stx)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(obj-id . _)
           (define rator (if (identifier? index/id)
                             index/id
                             #`(vector-ref (prop-methods-ref obj-id) #,index/id)))
           (syntax-parse stx
             #:datum-literals (op)
             #:literals (:=)
             [(head (op :=) . tail)
              #:with e::infix-op+expression+tail #'(:= . tail)
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (when (and r (eqv? 2 (method-result-arity r)))
                (raise-syntax-error #f
                                    (string-append "property does not support assignment" statically-str)
                                    id))
              (values #`(#,rator id e.parsed)
                      #'e.tail)]
             [(head . tail)
              (define call #`(#,rator obj-id))
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (values (add-method-result call r)
                      #'tail)])])))]
    [else
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
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (define-values (call new-tail)
                (parse-function-call rator (list #'id) #'(head args)
                                     #:static? (free-identifier=? (datum->syntax #'tag '#%call)
                                                                  #'static-#%call)
                                     #:rator-stx #'head
                                     #:rator-arity (and r (method-result-arity r))
                                     #:rator-kind 'method))
              (define wrapped-call (add-method-result call r))
              (values wrapped-call #'tail)])]
          [(head . _)
           (raise-syntax-error #f
                               (string-append "method must be called" statically-str)
                               #'head)])))]))
    

(define-for-syntax (build-methods method-results
                                  added-methods method-mindex method-names method-private
                                  names)
  (with-syntax ([(name name-instance name?
                       [field-name ...]
                       [field-static-infos ...]
                       [name-field ...]
                       [maybe-set-name-field! ...]
                       [private-field-name ...]
                       [private-field-desc ...]
                       [super-name ...])
                 names])
    (with-syntax ([(field-name ...) (for/list ([id/l (in-list (syntax->list #'(field-name ...)))])
                                      (if (identifier? id/l)
                                          (datum->syntax #'name (syntax-e id/l) id/l id/l)
                                          (car (syntax-e id/l))))]
                  [((method-name method-index/id method-result-id method-kind) ...)
                   (for/list ([i (in-range (hash-count method-mindex))])
                     (define raw-m-name (hash-ref method-names i))
                     (define m-name (if (syntax? raw-m-name)
                                        (syntax-e raw-m-name)
                                        raw-m-name))
                     (define mix (hash-ref method-mindex m-name))
                     ;; We use `raw-m-name` to support local references
                     ;; to macro-introduced methods
                     (list (datum->syntax #'name raw-m-name)
                           (mindex-index mix)
                           (let ([r (hash-ref method-results m-name #f)])
                             (and (pair? r) (car r)))
                           (if (mindex-property? mix) 'property 'method)))]
                  [((private-method-name private-method-id private-method-id/property private-method-result-id private-method-kind) ...)
                   (for/list ([m-name (in-list (sort (hash-keys method-private)
                                                     symbol<?))])
                     (define id/property (hash-ref method-private m-name))
                     (define id (if (pair? id/property) (car id/property) id/property))
                     (define raw-m-name (or (syntax-property id 'lhs-id) m-name))
                     ;; See above for explanation of `raw-m-name`
                     (list (datum->syntax #'name raw-m-name)
                           id
                           id/property
                           (let ([r (hash-ref method-results m-name #f)])
                             (and (pair? r) (car r)))
                           (if (pair? id/property) 'property 'method)))])
      (list
       #`(define-values #,(for/list ([added (in-list added-methods)]
                                     #:when (not (eq? 'abstract (added-method-body added))))
                            (added-method-rhs-id added))
           (let ()
             (define-syntax field-name (make-field-syntax (quote-syntax field-name)
                                                          (quote-syntax field-static-infos)
                                                          (quote-syntax name-field)
                                                          (quote-syntax maybe-set-name-field!)))
             ...
             (define-syntax method-name (make-method-syntax (quote-syntax method-name)
                                                            (quote-syntax method-index/id)
                                                            (quote-syntax method-result-id)
                                                            (quote method-kind)))
             ...
             (define-syntax private-method-name (make-method-syntax (quote-syntax private-method-name)
                                                                    (quote-syntax private-method-id)
                                                                    (quote-syntax private-method-result-id)
                                                                    (quote private-method-kind)))
             ...
             (define-syntax new-private-tables (cons (cons (quote-syntax name)
                                                           (hasheq (~@ 'private-method-name
                                                                       (quote-syntax private-method-id/property))
                                                                   ...
                                                                   (~@ 'private-field-name
                                                                       private-field-desc)
                                                                   ...))
                                                     (get-private-tables)))
             #,@(for/list ([added (in-list added-methods)]
                           #:when (eq? 'abstract (added-method-body added))
                           #:when (syntax-e (added-method-rhs added)))
                  #`(void (rhombus-expression #,(syntax-parse (added-method-rhs added)
                                                  [(_ rhs) #'rhs]))))
             (values
              #,@(for/list ([added (in-list added-methods)]
                            #:when (not (eq? 'abstract (added-method-body added))))
                   (define r (hash-ref method-results (syntax-e (added-method-id added)) #f))
                   #`(let ([#,(added-method-id added) (method-block #,(added-method-rhs added)
                                                                    name name-instance name?
                                                                    #,(and r (car r)) #,(added-method-id added)
                                                                    new-private-tables
                                                                    [super-name ...]
                                                                    #,(added-method-kind added))])
                       #,(added-method-id added))))))))))

(define-syntax (method-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(_ (block expr)
        name name-instance name?
        result-id method-name
        private-tables-id
        super-names
        kind)
     #:do [(define result-pred
             (cond
               [(not (syntax-e #'result-id)) #f]
               [else (method-result-predicate-expr (syntax-local-method-result #'result-id))]))]
     #:with (~var e (:entry-point (entry-point-adjustments
                                   (list #'this-obj)
                                   (lambda (arity stx)
                                     #`(syntax-parameterize ([this-id (quote-syntax (this-obj name-instance . super-names))]
                                                             [private-tables (quote-syntax private-tables-id)])
                                         ;; This check might be redundant, depending on how the method was called:
                                         (unless (name? this-obj) (raise-not-an-instance 'name this-obj))
                                         #,(let ([body #`(let ()
                                                           #,stx)])
                                             (cond
                                               [(and (eq? (syntax-e #'kind) 'property)
                                                     (eqv? arity 1))
                                                #`(begin #,body (void))]
                                               [result-pred
                                                #`(let ([result #,body])
                                                    (unless (#,result-pred result)
                                                      (raise-result-failure 'method-name result))
                                                    result)]
                                               [else body]))))
                                   #t)))
     #'expr
     #'e.parsed]))

