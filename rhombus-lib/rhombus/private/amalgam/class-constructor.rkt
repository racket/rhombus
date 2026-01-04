#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "class-parse.rkt"
                     "entry-point-adjustment.rkt")
         racket/unsafe/undefined
         racket/stxparam
         "parens.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         "class-this.rkt"
         "entry-point.rkt"
         (submod "annotation.rkt" for-class)
         "realm.rkt"
         "wrap-expression.rkt"
         (only-in "syntax-parameter.rkt"
                  syntax-parameters-key
                  with-syntax-parameters))

(provide (for-syntax build-class-constructor
                     need-class-constructor-wrapper?
                     encode-protocol
                     replace-public-protocol-with-arity
                     extract-constructor-arity)
         wrap-constructor)

;; Note: `constructor.macro` is handled in "class-dot.rkt"

;; About the constuctor protocol:
;;
;; In the simple case of no custom constructors and no private
;; immutable fields initialized from other initial fields values, then
;; a constructor can build a struct instance directly. Default-value
;; expressions recorded in per-class functions that can be chained
;; together in subclasses.
;;
;; When a class has a custom constructor or when it has private
;; immutable fields (where initialization can depend on other field
;; values), then subclass constructor composition will go through
;; `compose-constructor` to combine each subclass's constructor with
;; the superclass constructor one. That composition protocol relies on
;; currying, roughly
;;
;;   (lambda <super-args> ; protocol varies
;;      (lambda <sub-args>
;;        ....
;;        (lambda <all-super-args> ; includes resolved private and optionals
;;          (<final-make> @<all-super-args> @<sub-args>)))
;;
;; The protocol for <super-args> is the one defined by a custom
;; constrcutor, if any. If there's no custom constructor (i.e., just
;; private fields), then <super-args> includes those fields *prefixed*
;; by the superclass arguments. In other words, the way <super-args>
;; relates to the class's constructor arguments depends on the reason
;; for triggering `compose-constructor`. Also, the in latter case, the
;; implementation is known to accomodate `unsafe-undefined` for any
;; argument that is meant to be unsupplied, while an arbitrary custom
;; constructor may not support that (e.g., it may be impleemnted with
;; `case-lambda` where supplying `unsafe-undefined` would jump to the
;; wrong case).
;;
;; For an arbitrary custom constructor, arity information may be
;; available. If the arity is imple enough (e.g., no optional
;; arguments), then the outer wrapper to take <super-args> can be
;; synthesized precisely. Otherwise, `make-keyword-procedure-like` is
;; used dynamically to create a function that claims the same arity as
;; the superclass constructor. If the composition protocol is being
;; used only due to private fields (i.e., no custom constuctor), then
;; the outer wrapper always can be synthesized precisely, because
;; <super-args> can use `unsafe-undefined` for unsupplied optional
;; arguments.
;;
;; The inner step to take <all-super-args> always can be synthesized
;; directly, because its based on the actual fields of the class and
;; not on the constructor protocol.
;;
;; The information needed from superclasses to do all this is encoded
;; in the `constructor-makers` field of `class-desc`. That encoding
;; identifies a tail of the ancestor chain that uses default
;; constructors, where a combined one can be implemented directly, and
;; for the prefix of that chain, it describes to to build and call
;; composition functions. If arity information for a custom
;; constructor is available, it is recorded in `constructor-makers`
;; via `replace-public-protocol-with-arity`.
;;
;; The information encoded in `constructor-makers` records both the
;; constructor protocol (or as much as is known) and information about
;; private fields. When those have different shapes, then the encoding
;; has a vector or two encodings, one public and one private. The
;; public half of the encoding is in a box if it's for a custom
;; constructor (as opposed to just private fields), since that affects
;; how it relates to <super-args>. A nonnegative integer <n> means <n>
;; arguments with no optional or keyword arguments; a list indicates a
;; mixture of by-position and keyword arguments, where each argument
;; is a keyword or `#f` (for by-position) that is eiter boxed
;; (optional) or not (required).

(define-for-syntax (build-class-constructor super constructor-rhs constructor-stx-params
                                            added-fields constructor-exposures
                                            constructor-fields super-constructor-fields super-constructor+-fields
                                            keywords super-keywords super-constructor+-keywords
                                            defaults super-defaults super-constructor+-defaults
                                            constructor-field-static-infoss
                                            constructor-converters constructor-annotation-strs
                                            method-private
                                            need-constructor-wrapper?
                                            abstract-name internal-name
                                            has-defaults? super-has-defaults?
                                            final?
                                            names)
  (with-syntax ([(name make-name make-all-name constructor-name constructor-maker-name
                       visible-name
                       make-converted-name make-converted-internal
                       name?
                       name-defaults
                       make-internal-name
                       all-static-infos
                       [private-field-name ...]
                       [private-field-desc ...])
                 names])
    (define static-info-declss
      ;; static infos of previous constructor arguments to be used for each construct field
      ;; that has a default-value expression
      (let loop ([fields constructor-fields]
                 [static-infoss constructor-field-static-infoss]
                 [accum '()])
        (cond
          [(null? fields) '()]
          [else
           (cons accum
                 (loop (cdr fields)
                       (cdr static-infoss)
                       (cons #`(define-static-info-syntax/maybe #,(car fields)
                                 #,@(car static-infoss))
                             accum)))])))
    (define non-constructor-fields
      ;; recycling the names of the functions to call as binding for the result of the call
      (map added-field-arg-id added-fields))
    (define all-field-names (append constructor-fields non-constructor-fields))
    (define super-all-field-names (if super
                                      (append super-constructor+-fields
                                              (generate-temporaries
                                               (list-tail (or (class-desc-all-fields super)
                                                              (class-desc-fields super))
                                                          (length super-constructor+-fields))))
                                      null))
    (define (build-converted who f val converter annotation-str)
      (cond
        [(not (syntax-e converter)) val]
        [else #`(#,converter
                 (let ([#,f #,val]) #,f)
                 #,who
                 (lambda (val who)
                   (raise-annotation-failure who val '#,annotation-str)))]))
    (define constructor-field-tmps (generate-temporaries constructor-fields))
    (define (build-constructor-field-bindings who)
      (for/list ([f (in-list constructor-fields)]
                 [tmp (in-list constructor-field-tmps)]
                 [df (in-list defaults)]
                 [static-info-decls (in-list static-info-declss)]
                 [converter (in-list constructor-converters)]
                 [annotation-str (in-list constructor-annotation-strs)])
        (cond
          [(syntax-e df)
           #`[#,f #,(build-converted
                     who
                     f
                     #`(if (eq? #,tmp unsafe-undefined)
                           (let ()
                             #,@static-info-decls
                             (let ([#,f #,df]) #,f))
                           #,tmp)
                     converter
                     annotation-str)]]
          [else
           #`[#,f (let ()
                    #,@static-info-decls
                    #,(build-converted who f tmp converter annotation-str))]])))
    (define (build-added-field-bindings)
      (let loop ([non-constructor-fields non-constructor-fields]
                 [args constructor-fields])
        (cond
          [(null? non-constructor-fields) '()]
          [else
           (define id (car non-constructor-fields))
           (cons #`[#,id (#%plain-app #,id #,@args)]
                 (loop (cdr non-constructor-fields) (cons id args)))])))
    (append
     (if (syntax-e #'name-defaults)
         (list
          ;; default-value expressions should see only earlier fields
          ;; from `constructor-fields`, so use some temporary names
          ;; to make sure they can't be referenced
          (let* ([super-tmps (generate-temporaries super-constructor+-fields)]
                 [all-super-tmps (append super-tmps
                                         (generate-temporaries (list-tail super-all-field-names
                                                                          (length super-constructor+-fields))))])
            #`(define (name-defaults who #,@(append super-tmps constructor-field-tmps))
                (let-values #,(cond
                                [super-has-defaults?
                                 #`([#,all-super-tmps
                                     (#,(class-desc-defaults-id super) who . #,super-tmps)])]
                                [else '()])
                  (let* #,(append
                           (build-constructor-field-bindings #'who)
                           (build-added-field-bindings))
                    (values #,@all-super-tmps #,@constructor-fields #,@non-constructor-fields))))))
         null)
     (if need-constructor-wrapper?
         (list
          #`(define make-name
              (let ([name
                     (lambda #,(apply append (for/list ([f (in-list (append super-constructor+-fields constructor-field-tmps))]
                                                        [kw (in-list (append super-constructor+-keywords keywords))]
                                                        [df (in-list (append (cond
                                                                               [(and super
                                                                                     (class-desc-constructor-makers super))
                                                                                ;; no need for optional-argument defaults, because protocol
                                                                                ;; composition will supply `unsafe-undefined` for
                                                                                ;; missing arguments
                                                                                (map (lambda (v) #'#f) super-constructor+-defaults)]
                                                                               [else
                                                                                super-constructor+-defaults])
                                                                             defaults))])
                                               (let ([arg (if (syntax-e df)
                                                              #`[#,f unsafe-undefined]
                                                              f)])
                                                 (if (keyword? (syntax-e kw))
                                                     (list kw arg)
                                                     (list arg)))))
                       (let-values #,(cond
                                       [(syntax-e #'name-defaults)
                                        (define fields (append super-constructor+-fields constructor-field-tmps))
                                        (define all-fields (append super-all-field-names all-field-names))
                                        #`([#,all-fields (name-defaults 'name . #,fields)])]
                                       [super-has-defaults?
                                        #`([#,super-all-field-names
                                            (#,(class-desc-defaults-id super) 'name . #,super-constructor+-fields)])]
                                       [else '()])
                         (let* #,(if (syntax-e #'name-defaults)
                                     #'()
                                     (append
                                      (build-constructor-field-bindings #''name)
                                      (if has-defaults?
                                          (build-added-field-bindings)
                                          null)))
                           #,(if abstract-name
                                 #`(raise-abstract-methods 'name)
                                 #`(make-all-name #,@super-all-field-names
                                                  #,@all-field-names)))))])
                name)))
         null)
     (cond
       [constructor-rhs
        (define (encode-arity keywords defaults)
          (define-values (count opt req-kws allow-kws)
            (for/fold ([count 0] [opt 0] [req-kws '()] [allow-kws '()])
                      ([kw (in-list keywords)]
                       [def (in-list defaults)])
              (cond
                [(syntax-e kw)
                 (values count
                         opt
                         (if (syntax-e def) req-kws (cons kw req-kws))
                         (cons kw allow-kws))]
                [else
                 (values (add1 count)
                         (if (syntax-e def) (add1 opt) opt)
                         req-kws
                         allow-kws)])))
          (define mask (arithmetic-shift (sub1 (arithmetic-shift 1 (add1 opt))) (- count opt)))
          (if (null? allow-kws)
              mask
              (list mask req-kws allow-kws)))
        (define constructor-body
          #`(let ([visible-name
                   (syntax-parameterize
                       ([this-id
                         (quote-syntax (#:c #,(wrap-static-info*
                                               #'make-name
                                               #`((#%call-result
                                                   #,(let ([si #'all-static-infos])
                                                       (if super
                                                           #`((#%call-result #,si)
                                                              (#%function-arity #,(encode-arity keywords defaults)))
                                                           si)))
                                                  (#%function-arity #,(if super
                                                                          (or (class-desc-custom-constructor-maybe-arity super)
                                                                              (encode-arity super-keywords super-defaults))
                                                                          (encode-arity keywords defaults)))))))]
                        [private-tables
                         #,(with-syntax ([((private-method-name private-method-id/property) ...)
                                          (for/list ([m-name (in-list (sort (hash-keys method-private)
                                                                            symbol<?))])
                                            (define id/property (hash-ref method-private m-name))
                                            (list (datum->syntax #'name m-name)
                                                  id/property))])
                             #`(cons (cons (quote-syntax name)
                                           (hasheq (~@ 'private-method-name
                                                       (quote-syntax private-method-id/property))
                                                   ...
                                                   (~@ 'private-field-name
                                                       private-field-desc)
                                                   ...))
                                     (get-private-tables)))])
                     (wrap-constructor
                      name (#f) (name?)
                      #,(if (eq? constructor-rhs 'synthesize)
                            (make-default-constructor super
                                                      super-constructor-fields constructor-fields
                                                      super-keywords keywords
                                                      super-defaults defaults
                                                      constructor-exposures
                                                      #'make-name)
                            constructor-rhs)
                      #,constructor-stx-params))])
              visible-name))
        (cond
          [(and final?
                (not super))
           (list
            #`(define constructor-name
                #,constructor-body))]
          [else
           (list
            #`(define constructor-maker-name
                (lambda (make-name)
                  #,constructor-body))
            #`(define constructor-name
                #,(cond
                    [super
                     (compose-constructor
                      #'make-name
                      #`([#,(encode-protocol keywords defaults keywords defaults) name constructor-maker-name]
                         #,@(or (class-desc-constructor-makers super)
                                (list (list (encode-protocol super-keywords super-defaults
                                                             super-constructor+-keywords super-constructor+-defaults)
                                            (class-desc-id super))))))]
                    [else #'(constructor-maker-name make-name)])))])]
       [else null])
     (if (syntax-e #'make-converted-name)
         (list
          #`(define make-converted-name constructor-name))
         null)
     (if (syntax-e #'make-internal-name)
         (cond
           [(not super)
            ;; no superclass => can use `make-name` directly
            (list
             #`(define-syntax make-internal-name (make-rename-transformer (quote-syntax make-name))))]
           [(not (class-desc-constructor-makers super))
            ;; we can directly build the curried version
            (list
             #`(define make-internal-name
                 #,(make-curried-default-constructor super-constructor-fields constructor-fields
                                                     super-keywords keywords
                                                     super-defaults defaults
                                                     #'make-name
                                                     internal-name)))]
           [else
            ;; need to use `compose-constructor`
            (list
             #`(define make-internal-name
                 (let ([constructor-maker-name (lambda (make-name) make-name)])
                   #,(compose-constructor
                      #'make-name
                      #`([#,(encode-protocol keywords defaults keywords defaults) name constructor-maker-name]
                         #,@(class-desc-constructor-makers super))))))])
         null))))

(define-for-syntax (need-class-constructor-wrapper? extra-fields keywords defaults converters
                                                    constructor-rhs
                                                    has-private-constructor-fields?
                                                    super-has-keywords? super-has-defaults?
                                                    abstract-name
                                                    super)
  (or (pair? extra-fields)
      (any-stx? keywords)
      (any-stx? defaults)
      (any-stx? converters)
      (and (or super-has-keywords?
               super-has-defaults?)
           (or (not (class-desc-constructor-makers super))
               constructor-rhs
               has-private-constructor-fields?))
      abstract-name
      (and super
           (or (class-desc-all-fields super)
               (for/or ([f (in-list (class-desc-fields super))])
                 (identifier? (field-desc-constructor-arg f)))))))

(define-syntax (wrap-constructor stx)
  (syntax-parse stx
    #:datum-literals (parsed)
    [(_ name convert-id/args predicate-id/args (parsed #:rhombus/expr e) stx-params)
     #'(with-syntax-parameters stx-params e)]
    [(_ name (convert-id convert-arg ...) (predicate-id predicate-arg ...) (_::block g) stx-params)
     (define adjustments (entry-point-adjustment
                          (syntax-e #'name)
                          '()
                          (lambda (arity body)
                            #`(parsed
                               #:rhombus/expr
                               #,(let ([r #`(with-syntax-parameters
                                              stx-params
                                              #,(wrap-expression body))])
                                   (cond
                                     [(syntax-e #'convert-id)
                                      #`(let ([r #,r])
                                          (let ([get (convert-id r convert-arg ...)])
                                            (if get
                                                (get)
                                                #,(quasisyntax/loc #'g
                                                    (raise-constructor-result-error 'name r)))))]
                                     [(syntax-e #'predicate-id)
                                      #`(let ([r #,r])
                                          (if (predicate-id r predicate-arg ...)
                                              r
                                              #,(quasisyntax/loc #'g
                                                  (raise-constructor-result-error 'name r))))]
                                     [else r]))))
                          #f))
     (with-continuation-mark
      syntax-parameters-key #'stx-params
      (syntax-parse #'g
        [(~var lam (:entry-point adjustments))
         #'lam.parsed]))]))

(define-for-syntax (extract-constructor-arity stx stx-params)
  (syntax-parse stx
    [(_::block g)
     (with-continuation-mark
         syntax-parameters-key #'stx-params
         (syntax-parse #'g
           [(~var lam :entry-point-shape)
            (hash-ref (syntax->datum #'lam.parsed) 'arity #f)]))]))

(define (raise-constructor-result-error who val)
  (raise-arguments-error* who rhombus-realm
                          "constructor result does not satisfy annotation"
                          "result" val))

;; Beware that this function generates code that is quadratic in the
;; length of `makers` (i.e., in the depth of subclassing with custom
;; constructors). See "About the constuctor protocol" above for
;; general information.
(define-for-syntax (compose-constructor real-make makers)
  (define argss
    (let loop ([makers makers])
      (syntax-parse makers
        [([proto . _])
         (list (generate-protocol-formal-arguments #'proto))]
        [([proto . _] . rest)
         (define super-args (loop #'rest))
         (cons (append-protocols (car super-args)
                                 (generate-protocol-formal-arguments #'proto)
                                 (append-public-protocol? #'proto))
               super-args)])))
  (let loop ([makers makers]
             [argss argss]
             [final-make real-make])
    (syntax-parse makers
      [([proto name id:identifier])
       ;; root ancestor has a custom protocol
       #`(id #,final-make)]
      [([_ ancestor-name])
       ;; represents some number of ancestors that use the default protocol;
       ;; we don't expect to get here, though, since the last case below
       ;; looks ahead to cover this case
       (error "should not get here")]
      [([proto name id] . rest)
       ;; look ahead by one in `rest` so we can get the arity right
       (define formal-args (internal-protocol (generate-protocol-formal-arguments #'proto)))
       (define ancestor-formal-args (external-protocol (cadr argss)))
       (define ancestor-all-formal-args (internal-protocol (cadr argss)))
       (define args (protocol-formal->actual formal-args))
       (define ancestor-args (protocol-formal->actual ancestor-formal-args))
       (define ancestor-all-args (protocol-formal->actual ancestor-all-formal-args))
       (syntax-parse #'rest
         [([ancestor-proto ancestor-name])
          (with-syntax ([super-name (super-as-ancestor-name #'ancestor-name)])
            #`(id
               (let ([super-name
                      (lambda #,ancestor-formal-args
                        (lambda #,args
                          (#,final-make #,@ancestor-args #,@args)))])
                 super-name)))]
         [([ancestor-proto ancestor-name ancestor-id] . _)
          (define next
            (loop #'rest
                  (cdr argss)
                  #`(lambda #,ancestor-all-formal-args
                      (#,final-make #,@ancestor-all-args #,@args))))
          #`(id
             ;; The position after `constructor` is syntactically constrained, but its
             ;; arity is not necessarily statically known, and the protocol is recored
             ;; as #f if not or it it's too complex to generate a precise wrapper.
             ;; So, in the general case, we dynamically adjust a generic wrapper to
             ;; match the constructor's signature.
             #,(cond
                 [ancestor-args
                  ;; known and simple arity
                  (with-syntax ([super-name (super-as-ancestor-name #'ancestor-name)])
                    #`(let ([super-name
                             (lambda #,ancestor-formal-args
                               (lambda #,formal-args
                                 (#,next #,@ancestor-args)))])
                        super-name))]
                 [else
                  ;; general case
                  #`(make-keyword-procedure-like
                     ancestor-id
                     '#,(super-as-ancestor-name #'ancestor-name)
                     (lambda (kws kw-args . next-args)
                       (lambda #,args
                         (keyword-apply #,next
                                        kws kw-args
                                        next-args))))]))])])))

;; See "About the constuctor protocol" above
(define-for-syntax (encode-protocol keywords defaults
                                    +keywords +defaults)
  (define (encode keywords defaults)
    (for/list ([kw (in-list keywords)]
               [df (in-list defaults)])
      (if (syntax-e df)
          (box kw)
          kw)))
  (cond
    [(not (= (length keywords) (length +keywords)))
     (vector (encode keywords defaults)
             (encode +keywords +defaults))]
    [(for/and ([kw (in-list keywords)]
               [df (in-list defaults)])
       (and (not (syntax-e kw))
            (not (syntax-e df))))
     (length keywords)]
    [else
     (encode keywords defaults)]))

;; See "About the constuctor protocol" above
(define-for-syntax (replace-public-protocol-with-arity protocol arity)
  (define (encode-arity-as-protocol arity)
    (define (mask->n mask)
      ;; optional arguments have to be treated as complex, because
      ;; a function with operational arguments might not be
      ;; compatible with the hack of passing `unsafe-undefined`
      ;; for non-supplied arguments; for example, it might
      ;; be implemented with `case-lambda`, where different supplied
      ;; arguments jump to different cases
      (and (positive? mask)
           (= mask (arithmetic-shift 1 (sub1 (integer-length mask))))
           (sub1 (integer-length mask))))
    (cond
      [(eq? arity #t) ; => unknown
       #f]
      [(exact-integer? arity)
       (mask->n arity)]
      [else
       (define mask (list-ref arity 0))
       (cond
         [(mask->n mask)
          => (lambda (n)
               (define required-kws (list-ref arity 1))
               (define allowed-kws (list-ref arity 2))
               ;; Optional keywords have to be treated as complex for
               ;; the same reason that optional arguments are complex
               (cond
                 [(= (length allowed-kws) (length required-kws))
                  (append (for/list ([i (in-range n)])
                            #f)
                          (for/list ([kw (in-list allowed-kws)])
                            kw))]
                 [else
                  ;; rest keyword arguments are also too complex
                  #f]))]
         [else #f])]))
  (define (nonaccum v) (and v (box v)))
  (cond
    [(not arity)
     ;; arity corresponds to default constructor, so no replacement
     protocol]
    [(vector? protocol)
     (vector (nonaccum (encode-arity-as-protocol arity))
             ;; preserve private protocol:
             (vector-ref protocol 1))]
    [else
     (vector (nonaccum (encode-arity-as-protocol arity))
             ;; preserve constructed protocol as private protocol
             protocol)]))

(define-for-syntax (generate-protocol-formal-arguments proto)
  (let loop ([proto proto] [optionals? #t])
    (syntax-parse proto
      [#f #f]
      [count:exact-integer
       (generate-temporaries (for/list ([i (in-range (syntax-e #'count))]) i))]
      [#(proto internal-proto)
       (vector (loop #'proto #t)
               (loop #'internal-proto #f))]
      [#&proto (loop #'proto optionals?)]
      [(arg-desc ...)
       (apply append
              (for/list ([desc (in-list (syntax->list #'(arg-desc ...)))])
                (define id (car (generate-temporaries '(arg))))
                (define arg (if (and optionals?
                                     (box? (syntax-e desc)))
                                #`[#,id unsafe-undefined]
                                id))
                (cond
                  [(keyword? (syntax-e desc))
                   (list desc arg)]
                  [(and (box? (syntax-e desc))
                        (keyword? (syntax-e (unbox (syntax-e desc)))))
                   (list (unbox (syntax-e desc)) arg)]
                  [else (list arg)])))])))

(define-for-syntax (append-public-protocol? proto)
  (let loop ([proto proto])
    (syntax-parse proto
      [#f #f]
      [count:exact-integer #t]
      [#(proto internal-proto) (loop #'proto)]
      [#&proto #f]
      [(arg-desc ...) #t])))

(define-for-syntax (external-protocol v) (if (vector? v) (vector-ref v 0) v))
(define-for-syntax (internal-protocol v) (if (vector? v) (vector-ref v 1) v))
(define-for-syntax (append-protocols a b append-public?)
  (define (maybe-append a b) (and a b (append a b)))
  (cond
    [(vector? a) (if (vector? b)
                     (vector (if append-public?
                                 (maybe-append (vector-ref a 0) (vector-ref b 0))
                                  (vector-ref b 0))
                             (maybe-append (vector-ref a 1) (vector-ref b 1)))
                     (vector (if append-public?
                                 (maybe-append (vector-ref a 0) b)
                                 b)
                             (maybe-append (vector-ref a 1) b)))]
    [(vector? b) (vector (if append-public?
                             (maybe-append a (vector-ref b 0))
                             (vector-ref b 0))
                         (maybe-append a (vector-ref b 1)))]
    [else (if append-public?
              (maybe-append a b)
              (vector b
                      (maybe-append a b)))]))

(define-for-syntax (protocol-formal->actual args)
  (and args
       (for/list ([arg (in-list args)])
         (syntax-parse arg
           [(id . _) #'id]
           [_ arg]))))

(define-for-syntax (super-as-ancestor-name ancestor-name)
  (string->symbol (format "super to ~a" (syntax-e ancestor-name))))

(define (make-keyword-procedure-like make-proc name gen-proc)
  (define proc (make-proc void)) ; we know that `make-proc` immediately returns a procedure
  (define-values (allow-kws req-kws) (procedure-keywords proc))
  (procedure-reduce-keyword-arity-mask (make-keyword-procedure gen-proc)
                                       (procedure-arity-mask proc)
                                       allow-kws
                                       req-kws
                                       name
                                       rhombus-realm))

(define (raise-abstract-methods name)
  (raise-arguments-error* name rhombus-realm "cannot instantiate class with abstract methods"))

(define-for-syntax (make-default-constructor super
                                             super-constructor-fields constructor-fields
                                             super-keywords constructor-keywords
                                             super-defaults constructor-defaults
                                             constructor-exposures
                                             make-name)
  (define proto (encode-protocol constructor-keywords constructor-defaults
                                 constructor-keywords constructor-defaults))
  (define all-formal-args (generate-protocol-formal-arguments proto))
  (define (filter-map-arg proc)
    (let loop ([args all-formal-args] [exposures constructor-exposures])
      (cond
        [(null? args) '()]
        [(keyword? (syntax-e (car args))) (cons (car args) (loop (cdr args) exposures))]
        [else
         (define a (proc (car args) (not (eq? (car exposures) 'public))))
         (if a
             (cons a (loop (cdr args) (cdr exposures)))
             (loop (cdr args) (cdr exposures)))])))
  (define formal-args (filter-map-arg (lambda (arg private?)
                                        (and (not private?) arg))))
  (define all-args (filter-map-arg (lambda (arg private?)
                                     (if private?
                                         #'unsafe-undefined
                                         (syntax-parse arg
                                           [(id . _) #'id]
                                           [_ arg])))))
  (cond
    [(not super)
     #`(parsed
        #:rhombus/expr
        (lambda #,formal-args
          (#,make-name #,@all-args)))]
    [else
     (define super-proto (encode-protocol super-keywords super-defaults
                                          super-keywords super-defaults))
     (define super-formal-args (external-protocol (generate-protocol-formal-arguments super-proto)))
     (define super-all-args (protocol-formal->actual super-formal-args))
     #`(parsed
        #:rhombus/expr
        (lambda (#,@super-formal-args #,@formal-args)
          ((#,make-name #,@super-all-args) #,@all-args)))]))

(define-for-syntax (make-curried-default-constructor super-constructor-fields constructor-fields
                                                     super-keywords constructor-keywords
                                                     super-defaults constructor-defaults
                                                     make-name
                                                     internal-name)
  (define super-proto (encode-protocol super-keywords super-defaults
                                       super-keywords super-defaults))
  (define proto (encode-protocol constructor-keywords constructor-defaults
                                 constructor-keywords constructor-defaults))
  (define all-formal-args (generate-protocol-formal-arguments proto))
  (define all-args (protocol-formal->actual all-formal-args))
  (define super-formal-args (external-protocol (generate-protocol-formal-arguments super-proto)))
  (define super-all-args (protocol-formal->actual super-formal-args))
  (define curried-name (string->symbol (format "~a curried" (syntax-e internal-name))))
  (define super-name (string->symbol (format "~a super" (syntax-e internal-name))))
  #`(let ([#,curried-name
           (lambda (#,@super-formal-args)
             (let ([#,super-name (lambda (#,@all-formal-args)
                                   (#,make-name #,@super-all-args #,@all-args))])
               #,super-name))])
      #,curried-name))
