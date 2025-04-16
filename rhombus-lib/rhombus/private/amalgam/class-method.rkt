#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "class-parse.rkt"
                     "interface-parse.rkt"
                     "veneer-parse.rkt"
                     "class-method-result.rkt"
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "entry-point-adjustment.rkt"
                     "maybe-as-original.rkt")
         racket/stxparam
         "expression.rkt"
         (only-in "annotation.rkt"
                  :~)
         (only-in (submod "annotation.rkt" for-class)
                  annotation-predicate-form)
         "parse.rkt"
         "expression.rkt"
         "entry-point.rkt"
         "class-this.rkt"
         "class-this-id.rkt"
         "class-define-method-result.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "contains-key.rkt"
         "static-info.rkt"
         "indirect-static-info-key.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-provider-key.rkt"
         (submod "assign.rkt" for-assign)
         "parens.rkt"
         (submod "function-parse.rkt" for-call)
         "is-static.rkt"
         "realm.rkt"
         "name-prefix.rkt"
         "wrap-expression.rkt"
         (only-in "syntax-parameter.rkt"
                  with-syntax-parameters
                  syntax-parameters-key))

(provide (for-syntax extract-method-tables
                     build-interface-vtable
                     build-quoted-method-map
                     build-quoted-method-shapes
                     build-quoted-private-method-list
                     build-method-results
                     build-method-result-expression
                     build-methods

                     get-private-table
                     objects-desc-ref)

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
;;   method-private-inherit ; symbol -> (vector ref-id index maybe-result-id) for private methods overridden from interface
;;   method-decls    ; symbol -> identifier, intended for checking distinct
;;   abstract-name   ; #f or identifier for a still-abstract method

(define-for-syntax (extract-method-tables stx added-methods super interfaces
                                          private-interfaces protected-interfaces
                                          final? prefab?)
  (define supers (if super (cons super interfaces) interfaces))
  (define-values (super-str supers-str)
    (cond
      [(null? interfaces)
       (values "superclass" "superclasses(!?)")]
      [(not super)
       (values "interface" "superinterfaces")]
      [else
       (values "class or interface" "classes or superinterfaces")]))

  (define dot-ht
    (for*/fold ([ht #hasheq()])
               ([super (in-list supers)]
                [sym (in-list (objects-desc-dots super))])
      (when (hash-ref ht sym #f)
        (raise-syntax-error #f (format "dot syntax supplied by multiple ~a" supers-str) stx sym))
      (hash-set ht sym #t)))

  ;; create merged method tables from the superclass (if any) and all superinterfaces;
  ;; we start with the superclass, if any, so the methods from its vtable stay
  ;; in the same place in the new vtable
  (define-values (ht            ; symbol -> (cons mindex id)
                  super-priv-ht ; symbol -> identifier or (list identifier), implies not in `ht`
                  priv-inherit-ht ; symbol -> (vector ref-id index maybe-result-id)
                  vtable-ht     ; int -> accessor-identifier or '#:abstract
                  from-ht)      ; symbol -> super
    (for/fold ([ht #hasheq()] [priv-ht #hasheq()] [priv-inherit-ht #hasheq()] [vtable-ht #hasheqv()] [from-ht #hasheq()])
              ([super (in-list supers)])
      (define super-vtable (syntax-e (objects-desc-method-vtable super)))
      (define private? (hash-ref private-interfaces super #f))
      (define i-protected? (hash-ref protected-interfaces super #f))
      (for/fold ([ht ht] [priv-ht priv-ht] [priv-inherit-ht priv-inherit-ht] [vtable-ht vtable-ht] [from-ht from-ht])
                ([shape (in-vector (objects-desc-method-shapes super))]
                 [super-i (in-naturals)])
        (define new-rhs (let ([rhs (vector-ref super-vtable super-i)])
                          (if (eq? (syntax-e rhs) '#:abstract) '#:abstract rhs)))
        (define-values (key val i old-val m-final? protected?)
          (let* ([arity (and (vector? shape) (vector-ref shape 1))]
                 [shape (if (vector? shape) (vector-ref shape 0) shape)]
                 [protected? (protect? shape)]
                 [shape (if protected? (protect-v shape) shape)]
                 [property? (pair? shape)]
                 [shape (if (pair? shape) (car shape) shape)]
                 [final? (not (box? shape))]
                 [shape (if (box? shape) (unbox shape) shape)]
                 [protected? (or protected? i-protected?)])
            (define old-val (or (hash-ref ht shape #f)
                                (hash-ref priv-ht shape #f)))
            (define i (if (pair? old-val)
                          (mindex-index (car old-val))
                          (hash-count ht)))
            (values shape
                    (cons (mindex i final? protected? property? arity #t #f) shape)
                    i
                    old-val
                    final?
                    protected?)))
        (when (hash-ref dot-ht key #f)
          (raise-syntax-error #f (format "name supplied as both method and dot syntax by ~a" supers-str) stx key))
        (cond
          [old-val
           (define old-rhs (cond
                             [(and (pair? old-val)
                                   (mindex? (car old-val)))
                              (let ([old-i (car old-val)])
                                (hash-ref vtable-ht (mindex-index old-i)))]
                             [(pair? old-val) (car old-val)]
                             [else old-val]))
           (define (overridden?)
             (for/or ([added (in-list added-methods)])
               (and (eq? key (syntax-e (added-method-id added)))
                    (eq? 'override (added-method-replace added)))))
           (unless (or (if (identifier? old-rhs)
                           ;; same implementation?
                           (and (identifier? new-rhs)
                                (free-identifier=? old-rhs new-rhs))
                           ;; both abstract?
                           (eq? old-rhs new-rhs))
                       ;; from a common superclass/superinterface?
                       (and (not (and (identifier? new-rhs) (identifier? old-rhs)))
                            (in-common-superinterface (hash-ref from-ht key) super key))
                       ;; overridden
                       (overridden?))
             (raise-syntax-error #f (format "method supplied by multiple ~a and not overridden" supers-str) stx key))
           (define new-priv-inherit-ht
             (cond
               [(not private?) priv-inherit-ht]
               [(in-common-superinterface (hash-ref from-ht key) super key)
                => (lambda (common-super)
                     (hash-set priv-inherit-ht key (vector (interface-desc-internal-ref-id common-super)
                                                           super-i
                                                           (hash-ref (objects-desc-method-result common-super) key #f))))]
               [else
                (when (overridden?)
                  (raise-syntax-error #f "cannot override method from multiple privately implemented interfaces" stx key))
                priv-inherit-ht]))
           (unless (eq? (and protected? #t)
                        (and (pair? old-val)
                             (mindex? (car old-val))
                             (mindex-protected? (car old-val))
                             #t))
             (raise-syntax-error #f "method inherited as both protected and non-protected" stx key))
           (if (or private?
                   (and (pair? old-val) (mindex? (car old-val))
                        (not (and (identifier? new-rhs)
                                  (not (identifier? old-rhs)))))
                   (and (identifier? old-rhs)
                        (not (identifier? new-rhs))))
               (values ht priv-ht new-priv-inherit-ht vtable-ht from-ht)
               (values (hash-set ht key val)
                       (hash-remove priv-ht key)
                       (hash-remove priv-inherit-ht key)
                       (hash-set vtable-ht i new-rhs)
                       (hash-set from-ht key super)))]
          [private?
           (define (property-shape? shape) (or (pair? shape) (and (vector? shape) (pair? (vector-ref shape 0)))))
           (values ht
                   (hash-set priv-ht key (if (property-shape? shape)
                                             (list new-rhs)
                                             new-rhs))
                   (if m-final?
                       priv-inherit-ht
                       (hash-set priv-inherit-ht key (vector (interface-desc-internal-ref-id super)
                                                             super-i
                                                             (hash-ref (objects-desc-method-result super) key #f))))
                   vtable-ht
                   (hash-set from-ht key super))]
          [else
           (values (hash-set ht key val)
                   priv-ht
                   priv-inherit-ht
                   (hash-set vtable-ht i new-rhs)
                   (hash-set from-ht key super))]))))

  ;; merge method-result tables from superclass and superinterfaces,
  ;; assuming that the names all turn out to be sufficiently distinct
  (define super-method-results
    (for/fold ([method-results (if super
                                   (for/hasheq ([(sym id) (in-hash (objects-desc-method-result super))])
                                     (values sym (list id)))
                                   #hasheq())])
              ([intf (in-list interfaces)])
      (for/fold ([method-results method-results]) ([(sym id) (in-hash (objects-desc-method-result intf))])
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
    (for/fold ([ht ht] [vtable-ht vtable-ht] [priv-ht #hasheq()] [here-ht #hasheq()])
              ([added (in-list added-methods)])
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
      (when (hash-ref dot-ht (syntax-e id) #f)
        (raise-syntax-error #f (format "method name is supplied as dot syntax by ~a" super-str) stx id))
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
                 (define protected? (eq? (added-method-exposure added) 'protected))
                 (values (if (or final?
                                 (not (equal? (added-method-arity added) (mindex-arity mix))))
                             (let ([property? (eq? (added-method-kind added) 'property)]
                                   [arity (added-method-arity added)]
                                   [reflect-name (added-method-reflect-name added)])
                               (hash-set ht (syntax-e id) (cons (mindex idx final? protected? property? arity #f reflect-name) id)))
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
            (when prefab?
              (unless (eq? (added-method-disposition added) 'final)
                (raise-syntax-error #f "methods in a prefab class must be final" stx id)))
            (values (hash-set ht (syntax-e id)
                              (cons (mindex pos
                                            (or final?
                                                (eq? (added-method-disposition added) 'final))
                                            (eq? (added-method-exposure added) 'protected)
                                            (eq? (added-method-kind added) 'property)
                                            (added-method-arity added)
                                            #f
                                            (added-method-reflect-name added))
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
    (for/fold ([method-results super-method-results])
              ([added (in-list added-methods)]
               #:when (added-method-result-id added))
      (define sym (syntax-e (added-method-id added)))
      (hash-set method-results sym (cons (added-method-result-id added)
                                         (hash-ref method-results sym '())))))
  (define method-private
    (for/fold ([ht super-priv-ht]) ([(k v) (in-hash priv-ht)])
      (hash-set ht k v)))

  (define method-private-inherit priv-inherit-ht)

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
          method-private-inherit
          here-ht
          abstract-name))

(define-for-syntax (build-interface-vtable intf method-mindex method-vtable method-names method-private)
  (for/list ([shape (in-vector (objects-desc-method-shapes intf))])
    (define name (let* ([shape (if (vector? shape) (vector-ref shape 0) shape)]
                        [shape (if (protect? shape) (protect-v shape) shape)]
                        [shape (if (pair? shape) (car shape) shape)]
                        [shape (if (box? shape) (unbox shape) shape)])
                   shape))
    (cond
      [(hash-ref method-private name #f)
       => (lambda (id) (if (pair? id) (car id) id))]
      [else
       (define pos (mindex-index (hash-ref method-mindex name)))
       (define n (vector-ref method-vtable pos))
       (if (keyword? n)
           #`(quote #,n)
           n)])))

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
    (define vsym (if (mindex-protected? mix) (protect sym) sym))
    (if (mindex-arity mix)
        (vector vsym (mindex-arity mix))
        vsym)))

(define-for-syntax (build-quoted-private-method-list mode method-private)
  (sort (for/list ([(sym v) (in-hash method-private)]
                   #:when (eq? mode (if (pair? v) 'property 'method)))
          sym)
        symbol<?))

(define-for-syntax (build-method-results added-methods
                                         method-mindex method-vtable method-private
                                         method-results
                                         in-final?
                                         methods-ref-id
                                         call-statinfo-indirect-stx callable?
                                         index-statinfo-indirect-stx indexable?
                                         index-set-statinfo-indirect-stx setable?
                                         append-statinfo-indirect-stx appendable?
                                         compare-statinfo-indirect-stx comparable?
                                         contains-statinfo-indirect-stx container?
                                         super-call-statinfo-indirect-id
                                         #:checked-append? [checked-append? #t]
                                         #:checked-compare? [checked-compare? #t])
  (define defs
    (for/list ([added (in-list added-methods)])
      #`(define-method-result #,(added-method-result-id added)
          #,(added-method-maybe-ret added)
          #,(added-method-ret-forwards added)
          #,(cdr (hash-ref method-results (syntax-e (added-method-id added)) '(none)))
          ;; Also add static info as #%call-result to binding; use the method-result id
          ;; to hold this information if there's implementation name to attach it to
          #,(or (let ([id/property (hash-ref method-private (syntax-e (added-method-id added)) #f)])
                  (if (pair? id/property) (car id/property) id/property))
                (let ([mix (hash-ref method-mindex (syntax-e (added-method-id added)) #f)])
                  (define id (vector-ref method-vtable (mindex-index mix)))
                  (if (eq? id '#:abstract)
                      (added-method-result-id added)
                      id)))
          ;; result annotation can convert if final
          #,(or in-final?
                (eq? (added-method-disposition added) 'final))
          #,checked-append?
          #,checked-compare?
          #,(added-method-kind added)
          #,(added-method-arity added)
          #,(and callable?
                 (eq? 'call (syntax-e (added-method-id added)))
                 call-statinfo-indirect-stx)
          #,(and indexable?
                 (eq? 'get (syntax-e (added-method-id added)))
                 #`[#,index-statinfo-indirect-stx #,(added-method-rhs-id added)])
          #,(and setable?
                 (eq? 'set (syntax-e (added-method-id added)))
                 #`[#,index-set-statinfo-indirect-stx #,(added-method-rhs-id added)])
          #,(and appendable?
                 (eq? 'append (syntax-e (added-method-id added)))
                 #`[#,append-statinfo-indirect-stx #,(added-method-rhs-id added)])
          #,(and comparable?
                 (eq? 'compare_to (syntax-e (added-method-id added)))
                 #`[#,compare-statinfo-indirect-stx
                    #,(cond
                        [checked-compare?
                         '#:method]
                        [else
                         ;; veneer case: need to name methods explicitly, since
                         ;; there's no vtable associated with an instance
                         (let ([methods '(less less_or_equal
                                               compares_equal compares_unequal
                                               greater_or_equal greater)]
                               [ops '(< <= = != >= >)]
                               [addeds (for/hasheq ([a (in-list added-methods)])
                                         (values (syntax-e (added-method-id a)) a))])
                           (for/list ([name (in-list methods)]
                                      [op (in-list ops)])
                             #`(#,op #,(or (let ([a (hash-ref addeds name #f)])
                                             (and a (added-method-rhs-id a)))
                                           (box (added-method-rhs-id added))))))])])
          #,(and container?
                 (eq? 'contains (syntax-e (added-method-id added)))
                 #`[#,contains-statinfo-indirect-stx #,(added-method-rhs-id added)]))))

  ;; may need to add info for inherited `call`, etc.:
  (define (add-able which statinfo-indirect-stx able? key defs abstract-args
                    #:use-id [use-id #f]
                    #:box-id? [box-id? #f]
                    #:const [const #f])
    (if (and statinfo-indirect-stx
             able?
             (not (for/or ([added (in-list added-methods)])
                    (eq? which (syntax-e (added-method-id added))))))
        ;; method is inherited, so bounce again to inherited method's info
        (let* ([index (mindex-index (hash-ref method-mindex which))]
               [impl-id (or use-id (vector-ref method-vtable index))])
          (define abstract? (eq? impl-id '#:abstract))
          (if (or (not abstract?) abstract-args)
              (cons
               #`(define-static-info-syntax #,statinfo-indirect-stx
                   (#,key #,(cond
                              [const const]
                              [abstract?
                               #`(lambda (obj . #,abstract-args)
                                   ((method-ref #,methods-ref-id obj #,index) obj . #,abstract-args))]
                              [box-id? (box-immutable impl-id)]
                              [else impl-id])))
               defs)
              defs))
        defs))
  (let* ([defs (add-able 'call call-statinfo-indirect-stx callable? #'#%indirect-static-info defs #f
                         #:use-id super-call-statinfo-indirect-id)]
         [defs (add-able 'get index-statinfo-indirect-stx indexable? #'#%index-get defs #'(index))]
         [defs (add-able 'set index-set-statinfo-indirect-stx setable? #'#%index-set defs #'(index val))]
         [defs (add-able 'append append-statinfo-indirect-stx appendable? #'#%append defs #'(val)
                         ;; boxed means "checked" for `#%append`:
                         #:box-id? checked-append?)]
         [defs (add-able 'compare_to compare-statinfo-indirect-stx comparable? #'#%compare defs #'(val)
                         #:const '#:method)]
         [defs (add-able 'contains contains-statinfo-indirect-stx container? #'#%contains defs #'(val))])
    defs))

(define-for-syntax (build-method-result-expression method-result)
  #`(hasheq
     #,@(apply append
               (for/list ([(sym ids) (in-hash method-result)])
                 (list #`(quote #,sym)
                       #`(quote-syntax #,(car ids)))))))

(define-for-syntax (in-common-superinterface i j key)
  (define (lookup id)
    (and id (syntax-local-value* (in-class-desc-space id)
                                 (lambda (v)
                                   (or (class-desc-ref v)
                                       (interface-desc-ref v))))))
  (define (gather-from-interfaces int-ids ht saw-abstract?)
    (let ([int-ids (syntax->list int-ids)])
      (for/fold ([ht ht]) ([int-id (in-list int-ids)])
        (gather (lookup int-id) ht saw-abstract?))))
  (define (gather i ht saw-abstract?)
    (cond
      [(and i (hash-has-key? (objects-desc-method-map i) key))
       (define idx (hash-ref (objects-desc-method-map i) key))
       (define impl (vector-ref (syntax-e (objects-desc-method-vtable i)) idx))
       (cond
         [(and (not (eq? (syntax-e impl) '#:abstract))
               saw-abstract?)
          ;; no superinterface abstract is the relevant abstract
          ht]
         [else
          (define new-saw-abstract? (or saw-abstract? (eq? (syntax-e impl) '#:abstract)))
          (gather-from-interfaces (objects-desc-interface-ids i)
                                  (let ([ht (hash-set ht i #t)])
                                    (if (class-desc? i)
                                        (gather (lookup (class-desc-super-id i))
                                                ht
                                                new-saw-abstract?)
                                        ht))
                                  new-saw-abstract?)])]
      [else ht]))
  (define i-ht (gather i #hasheq() #f))
  (define j-ht (gather j #hasheq() #f))
  (for/or ([k (in-hash-keys i-ht)])
    (and (hash-ref j-ht k #f) k)))

(define-syntax super
  (expression-transformer
   (lambda (stxs)
     (syntax-parse stxs
       [(head . tail)
        (define c-or-id+dp+isi+supers (syntax-parameter-value #'this-id))
        (unless c-or-id+dp+isi+supers
          (raise-syntax-error #f
                              "allowed only within methods and constructors"
                              #'head))
        (syntax-parse c-or-id+dp+isi+supers
          [(_:keyword make-name)
           ;; in a constructor
           (values (relocate+reraw #'head #'make-name) #'tail)]
          [(id dp isi . super-ids)
           ;; in a method
           (when (null? (syntax-e #'super-ids))
             (raise-syntax-error #f "class has no superclass" #'head))
           (syntax-parse #'tail
             #:datum-literals (op |.|)
             [((op (~and dot-op |.|)) method-id:identifier . tail)
              (define super+pos
                (for/fold ([found #f]) ([super-id (in-list (syntax->list #'super-ids))])
                  (define super (syntax-local-value* (in-class-desc-space super-id)
                                                     (lambda (v)
                                                       (or (class-desc-ref v)
                                                           (interface-desc-ref v)))))
                  (unless super
                    (raise-syntax-error #f "class or interface not found" super-id))
                  (define pos (hash-ref (objects-desc-method-map super) (syntax-e #'method-id) #f))
                  (when found
                    (unless (in-common-superinterface (car found) super (syntax-e #'method-id))
                      (raise-syntax-error #f "inherited method is ambiguous" #'method-id)))
                  (and pos (cons super pos))))
              (unless super+pos
                (raise-syntax-error #f "no such method in superclass" #'head #'method-id))
              (define super (car super+pos))
              (define pos (cdr super+pos))
              (define impl (vector-ref (syntax-e (objects-desc-method-vtable super)) pos))
              (when (eq? (syntax-e impl) '#:abstract)
                (raise-syntax-error #f "method is abstract in superclass" #'head #'method-id))
              (define shape+arity (vector-ref (objects-desc-method-shapes super) pos))
              (define shape-ex (if (vector? shape+arity) (vector-ref shape+arity 0) shape+arity))
              (define shape (if (protect? shape-ex) (protect-v shape-ex) shape-ex))
              (define shape-arity (and (vector? shape+arity) (vector-ref shape+arity 1)))
              (define static? (is-static-context? #'dot-op))
              (cond
                [(pair? shape)
                 ;; a property
                 (syntax-parse #'tail
                   [assign::assign-op-seq
                    (define-values (assign-call assign-empty-tail to-anon-function?)
                      (parse-function-call impl (list #'id #'v) #'(method-id (parens))
                                           #:static? static?
                                           #:rator-stx #'head
                                           #:rator-kind 'property
                                           #:rator-arity shape-arity))
                    (define-values (assign-expr tail) (build-assign
                                                       (attribute assign.op)
                                                       #'assign.op-name
                                                       #'assign.name
                                                       #`(lambda () (#,impl obj))
                                                       #`(lambda (v) #,assign-call)
                                                       #'obj
                                                       #'assign.tail))
                    (values #`(let ([obj id])
                                #,assign-expr)
                            tail)]
                   [_
                    (define-values (call new-tail to-anon-function?)
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
                    (define-values (call new-tail to-anon-function?)
                      (parse-function-call impl (list #'id) #'(method-id args)
                                           #:static? static?
                                           #:rator-stx #'head
                                           #:rator-kind 'method
                                           #:rator-arity shape-arity
                                           #:can-anon-function? #t))
                    (values call #'tail)])])])])]))))

(define-for-syntax (objects-desc-ref v)
  (or (class-desc-ref v)
      (interface-desc-ref v)
      (veneer-desc-ref v)))

(define-for-syntax (get-private-table desc
                                      #:fail-v [fail-v #hasheq()]
                                      #:allow-super-for [allow-super-for-sym #f])
  (define tables (get-private-tables))
  (or (for/or ([t (in-list tables)])
        (define id (car t))
        (define (match? id)
          (free-identifier=? id (cond
                                  [(class-desc? desc)
                                   (class-desc-id desc)]
                                  [(interface-desc? desc)
                                   (interface-desc-id desc)]
                                  [else
                                   (veneer-desc-id desc)])))
        (and (cond
               [(match? id) #t]
               [allow-super-for-sym
                ;; This mode is used for `protected` access. Check
                ;; whether we're in a subclass/subinterface of the
                ;; needed one, or check whether the relevant method or
                ;; field is inherited from a common ancestor.
                ;; This is potentially slow for a large inheritance tree,
                ;; but that seems like an unlikely problem in the near
                ;; term; we do at least use a `seen` table to handle DAGs
                ;; without unfolding them to trees.
                (define (get id)
                  (syntax-local-value* (in-class-desc-space id)
                                       objects-desc-ref))
                (define relevant-ancestor-descs
                  (let loop ([desc desc] [accum #hasheq()])
                    (define (check id accum)
                      (define desc (get id))
                      (if (and (has-protected-method-or-field? desc allow-super-for-sym)
                               (not (hash-ref accum desc #f)))
                          (loop desc (hash-set accum desc #t))
                          accum))
                    (let ([accum (if (and (class-desc? desc)
                                          (class-desc-super-id desc))
                                     (check (class-desc-super-id desc) accum)
                                     accum)])
                      (for/fold ([accum accum])
                                ([id (in-list (syntax->list (objects-desc-interface-ids desc)))])
                        (check id accum)))))
                (define seen (make-hasheq))
                (define (search-id id)
                  (or (match? id)
                      (search-desc (get id))))
                (define (search-desc desc)
                  (and desc
                       (or (hash-ref relevant-ancestor-descs desc #f)
                           (and (not (hash-ref seen desc #f))
                                (begin
                                  (hash-set! seen desc #t)
                                  (or (and (class-desc? desc)
                                           (or (and (class-desc-super-id desc)
                                                    (search-id (class-desc-super-id desc)))
                                               (and (class-desc-protected-interface-ids desc)
                                                    (for/or ([id (in-list (syntax->list (class-desc-protected-interface-ids desc)))])
                                                      (search-id id)))))
                                      (for/or ([id (in-list (syntax->list (objects-desc-interface-ids desc)))])
                                        (search-id id))))))))
                (search-id id)]
               [else #f])
             (cdr t)))
      fail-v))

;; ok to return #t for pubic, but only obliged to look for protected
(define-for-syntax (has-protected-method-or-field? desc sym)
  (or (hash-ref (objects-desc-method-map desc) sym #f)
      (and (class-desc? desc)
           (class-desc-all-fields desc)
           (for/or ([a-field (in-list (class-desc-all-fields desc))])
             (and (protect? a-field)
                  (eq? sym (field-desc-name (protect-v a-field))))))))

(define-for-syntax (make-field-syntax id static-infos accessor-id maybe-mutator-id)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(head . tail)
        #:when (syntax-e maybe-mutator-id)
        #:with assign::assign-op-seq #'tail
        (syntax-parse (syntax-parameter-value #'this-id)
          [(obj-id . _)
           (build-assign (attribute assign.op)
                         #'assign.op-name
                         #'assign.name
                         #`(lambda () (#,accessor-id obj-id))
                         #`(lambda (v) (#,maybe-mutator-id obj-id v))
                         #'id
                         #'assign.tail)])]
       [(head . tail)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(id . _)
           (values (wrap-static-info* (datum->syntax #'here
                                                     (list accessor-id #'id)
                                                     #'head
                                                     #'head)
                                      static-infos)
                   #'tail)])]))))

(define-for-syntax (make-method-syntax id index/id/intf result-id kind methods-ref-id)
  (define (add-method-result call r)
    (if r
        (wrap-static-info* call (method-result-static-infos r))
        call))
  (cond
    [(eq? kind 'property)
     (expression-transformer
      (lambda (stx)
        (syntax-parse (syntax-parameter-value #'this-id)
          [(obj-id . _)
           (define rator (cond
                           [(identifier? index/id/intf) index/id/intf]
                           [(vector? (syntax-e index/id/intf))
                            (define-values (ref-id pos result-id) (unpack-intf-ref index/id/intf))
                            #`(vector-ref (#,ref-id obj-id) #,pos)]
                           [else
                            #`(vector-ref (#,methods-ref-id obj-id) #,index/id/intf)]))
           (syntax-parse stx
             [(head . tail)
              #:with assign::assign-op-seq #'tail
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (when (and r (eqv? 2 (method-result-arity r)))
                (raise-syntax-error #f
                                    (string-append "property does not support assignment" statically-str)
                                    id))
              (build-assign (attribute assign.op)
                            #'assign.op-name
                            #'assign.name
                            #`(lambda () (#,rator obj-id))
                            #`(lambda (v) (#,rator obj-id v))
                            #'id
                            #'assign.tail)]
             [(head . tail)
              (define call (relocate+reraw #'head #`(#,rator obj-id)))
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (values (add-method-result call r)
                      #'tail)])])))]
    [else
     (expression-transformer
      (lambda (stx)
        (syntax-parse stx
          [(head (~and args (tag::parens arg ...)) . tail)
           (syntax-parse (syntax-parameter-value #'this-id)
             [(id . _)
              (define rator (cond
                              [(identifier? index/id/intf)
                               index/id/intf]
                              [(vector? (syntax-e index/id/intf))
                               (define-values (ref-id pos result-id) (unpack-intf-ref index/id/intf))
                               #`(vector-ref (#,ref-id id) #,pos)]
                              [else
                               #`(vector-ref (#,methods-ref-id id) #,index/id/intf)]))
              (define r (and (syntax-e result-id)
                             (syntax-local-method-result result-id)))
              (define-values (call new-tail to-anon-function?)
                (parse-function-call rator (list #'id) #'(head args)
                                     #:static? (is-static-context? #'tag)
                                     #:rator-stx #'head
                                     #:rator-arity (and r (method-result-arity r))
                                     #:rator-kind 'method
                                     #:can-anon-function? #t))
              (define wrapped-call (if to-anon-function?
                                       call
                                       (add-method-result call r)))
              (values wrapped-call #'tail)])]
          [(head . _)
           (raise-syntax-error #f
                               (string-append "method must be called" statically-str)
                               #'head)])))]))


(define-for-syntax (build-methods method-results
                                  added-methods method-mindex method-names method-private method-private-inherit
                                  reconstructor-rhs reconstructor-stx-params serializer-stx-params in-final?
                                  private-interfaces protected-interfaces
                                  names
                                  #:veneer-vtable [veneer-vtable #f])
  (with-syntax ([(name reflect-name name-instance name? name-convert reconstructor-name serializer-name
                       methods-ref
                       indirect-static-infos
                       [field-name ...]
                       [field-static-infos ...]
                       [name-field ...]
                       [maybe-set-name-field! ...]
                       [private-field-name ...]
                       [private-field-desc ...]
                       [(super-protected-field-name
                         super-protected-name-field
                         super-protected-maybe-set-name-field!
                         super-protected-field-static-infos
                         super-protected-constructor-arg)
                        ...]
                       [super-name ...]
                       [(recon-field-accessor recon-field-rhs) ...]
                       serializable)
                 names])
    (with-syntax ([(field-name ...) (for/list ([id/l (in-list (syntax->list #'(field-name ...)))])
                                      (if (identifier? id/l)
                                          (datum->syntax #'name (syntax-e id/l) id/l id/l)
                                          (car (syntax-e id/l))))]
                  [(super-protected-field-name ...) (for/list ([id (in-list (syntax->list #'(super-protected-field-name ...)))])
                                                      (datum->syntax #'name (syntax-e id) id id))]
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
                           (let ([idx (mindex-index mix)])
                             (if veneer-vtable
                                 ;; always static:
                                 (vector-ref veneer-vtable idx)
                                 idx))
                           (let ([r (hash-ref method-results m-name #f)])
                             (and (pair? r) (car r)))
                           (if (mindex-property? mix) 'property 'method)))]
                  [((private-method-name private-method-id/intf private-method-id/intf/property private-method-result-id private-method-kind) ...)
                   (for/list ([m-name (in-list (sort (hash-keys method-private)
                                                     symbol<?))])
                     (define id/property (hash-ref method-private m-name))
                     (define property? (pair? id/property))
                     (define-values (id/intf/property result-id)
                       (cond
                         [(and (not in-final?)
                               (hash-ref method-private-inherit m-name #f))
                          => (lambda (vec)
                               ;; Override of private implemented, so we need to access internally
                               ;; by interface vtable in case it's overridden in a subclass; a vector
                               ;; encodes a property reference and vtable index
                               (values (if property?
                                           (list vec)
                                           vec)
                                       (vector-ref vec 2)))]
                         [else (values id/property
                                       (let ([r (hash-ref method-results m-name #f)])
                                         (and (pair? r) (car r))))]))
                     (define id/intf (if property? (car id/intf/property) id/intf/property))
                     (define id (if property? (car id/property) id/property))
                     (define raw-m-name (or (syntax-property id 'lhs-id) m-name))
                     ;; See above for explanation of `raw-m-name`
                     (list (datum->syntax #'name raw-m-name)
                           id/intf
                           id/intf/property
                           result-id
                           (if property? 'property 'method)))]
                  [(private-interface-name ...) (append (for/list ([intf (in-hash-keys private-interfaces)])
                                                          (interface-desc-id intf)))])
      (list
       #`(define-values (#,@(for/list ([added (in-list added-methods)]
                                       #:when (not (eq? 'abstract (added-method-body added))))
                              (maybe-as-original (added-method-rhs-id added)
                                                 (added-method-id added)))
                         #,@(if (and (syntax-e #'reconstructor-name)
                                     (not (eq? reconstructor-rhs 'default)))
                                (list #'reconstructor-name)
                                null)
                         #,@(if (syntax-e #'serializer-name)
                                (list #'serializer-name)
                                null)
                         #,@(for/list ([acc (in-list (syntax->list #'(recon-field-accessor ...)))]
                                       [rhs (in-list (syntax->list #'(recon-field-rhs ...)))]
                                       #:when (syntax-e rhs))
                              acc))
           (let ()
             (define-syntax field-name (make-field-syntax (quote-syntax field-name)
                                                          (quote-syntax field-static-infos)
                                                          (quote-syntax name-field)
                                                          (quote-syntax maybe-set-name-field!)))
             ...
             (define-syntax super-protected-field-name (make-field-syntax (quote-syntax super-protected-field-name)
                                                                          (quote-syntax super-protected-field-static-infos)
                                                                          (quote-syntax super-protected-name-field)
                                                                          (quote-syntax super-protected-maybe-set-name-field!)))
             ...
             (define-syntax method-name (make-method-syntax (quote-syntax method-name)
                                                            (quote-syntax method-index/id)
                                                            (quote-syntax method-result-id)
                                                            (quote method-kind)
                                                            (quote-syntax methods-ref)))
             ...
             (define-syntax private-method-name (make-method-syntax (quote-syntax private-method-name)
                                                                    (quote-syntax private-method-id/intf)
                                                                    (quote-syntax private-method-result-id)
                                                                    (quote private-method-kind)
                                                                    (quote-syntax methods-ref)))
             ...
             (define-syntax new-private-tables (list* (cons (quote-syntax name)
                                                            (hasheq (~@ 'private-method-name
                                                                        (quote-syntax private-method-id/intf/property))
                                                                    ...
                                                                    (~@ 'private-field-name
                                                                        private-field-desc)
                                                                    ...))
                                                      (cons (quote-syntax private-interface-name) ; for access to protected members
                                                            #hasheq())
                                                      ...
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
                   (define method-name (let ([id (added-method-id added)])
                                         ;; disable check-syntax presence:
                                         (datum->syntax id (syntax-e id) #f)))
                   #`(let ([#,method-name (method-block #,(added-method-rhs added) #,(added-method-stx-params added)
                                                        reflect-name name name-instance name? name-convert
                                                        #,(and r (car r)) #,(added-method-id added)
                                                        new-private-tables
                                                        indirect-static-infos
                                                        [super-name ...]
                                                        #,(added-method-kind added))])
                       #,method-name))
              #,@(if (and (syntax-e #'reconstructor-name)
                          (not (eq? reconstructor-rhs 'default)))
                     (list
                      #`(method-block (block #,reconstructor-rhs) #,reconstructor-stx-params
                                      reflect-name name name-instance #f #f
                                      #f reconstructor
                                      new-private-tables
                                      indirect-static-infos
                                      ()
                                      reconstructor))
                     null)
              #,@(if (syntax-e #'serializer-name)
                     (list
                      (syntax-parse #'serializable
                        [(_ _ serializer-rhs . _)
                         #`(method-block (block serializer-rhs) #,serializer-stx-params
                                         reflect-name name name-instance #f #f
                                         #f serializer
                                         new-private-tables
                                         indirect-static-infos
                                         ()
                                         serializer)]))
                     null)
              #,@(for/list ([acc (in-list (syntax->list #'(recon-field-accessor ...)))]
                            [rhs (in-list (syntax->list #'(recon-field-rhs ...)))]
                            #:when (syntax-e rhs))
                   #`(method-block (block #,rhs) #f ;; FIXME
                                   reflect-name name name-instance #f #f
                                   #f acc
                                   new-private-tables
                                   indirect-static-infos
                                   ()
                                   reconstructor_field)))))))))

(define-syntax (method-block stx)
  (syntax-parse stx
    [(_ (_::block expr) stx-params
        reflect-name name name-instance name? name-convert
        result-id method-name
        private-tables-id
        indirect-static-infos
        super-names
        kind)
     #:with prefixed-method-name (add-name-prefix #'reflect-name #'method-name)
     (define result-desc
       (cond
         [(eq? (syntax-e #'kind) 'serializer)
          (method-result #'vector? #t 1 "Array" #'() 1)]
         [(not (syntax-e #'result-id)) #f]
         [else (syntax-local-method-result #'result-id)]))
     (with-continuation-mark
      syntax-parameters-key #'stx-params
      (syntax-parse #'expr
        [(~var e (:entry-point (entry-point-adjustment
                                (syntax-e #'prefixed-method-name)
                                (list #'this-obj)
                                (lambda (arity stx)
                                  #`(parsed
                                     #:rhombus/expr
                                     #,(let ()
                                         (define (wrap body)
                                           ;; The wrapped check might be redundant, depending on how the method was called
                                           (cond
                                             [(syntax-e #'name-convert)
                                              #`(let ([this-obj (name-convert this-obj 'prefixed-method-name)])
                                                  #,body)]
                                             [(syntax-e #'name?)
                                              #`(begin
                                                  (unless (name? this-obj) (raise-not-an-instance 'prefixed-method-name this-obj))
                                                  #,body)]
                                             [else
                                              body]))
                                         (wrap
                                          #`(syntax-parameterize ([this-id (quasisyntax (this-obj name-instance
                                                                                                  ;; can include `unsyntax`:
                                                                                                  indirect-static-infos
                                                                                                  . super-names))]
                                                                  [private-tables (quote-syntax private-tables-id)])
                                              #,(let ([body #`(with-syntax-parameters
                                                                stx-params
                                                                (let ()
                                                                  #,(wrap-expression stx)))])
                                                  (cond
                                                    [(and (eq? (syntax-e #'kind) 'property)
                                                          (eqv? arity 2)) ; mask 2 => 1 argument
                                                     #`(begin #,body (void))]
                                                    [(and result-desc
                                                          (method-result-handler-expr result-desc))
                                                     => (lambda (proc)
                                                          (wrap-annotation-check
                                                           #'prefixed-method-name body
                                                           (method-result-count result-desc)
                                                           (method-result-annot-str result-desc)
                                                           (lambda (vs raise)
                                                             (if (method-result-predicate? result-desc)
                                                                 #`(begin
                                                                     (unless (#,proc #,@vs) #,raise)
                                                                     (values #,@vs))
                                                                 #`(#,proc
                                                                    #,@vs
                                                                    (lambda (#,@vs) (values #,@vs))
                                                                    (lambda () #,raise))))))]
                                                    [else body])))))))
                                #t)))
         #'e.parsed]))]))
