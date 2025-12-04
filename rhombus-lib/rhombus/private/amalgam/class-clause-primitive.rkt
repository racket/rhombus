#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/name-parse
                     "name-path-op.rkt"
                     "class-parse.rkt"
                     "consistent.rkt"
                     "srcloc.rkt")
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         (submod "class-clause.rkt" for-class)
         "interface-clause.rkt"
         "veneer-clause.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         "parens.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "var-decl.rkt"
         (only-in "function.rkt" fun)
         (submod "function.rkt" for-method)
         (submod "values.rkt" for-parse)
         "op-literal.rkt"
         "not-block.rkt"
         "realm.rkt")

(provide (for-space rhombus/class_clause
                    nonfinal
                    opaque
                    prefab
                    authentic
                    field
                    immutable
                    constructor
                    reconstructor
                    reconstructor_fields
                    serializable)
         (for-spaces (rhombus/class_clause
                      rhombus/veneer_clause)
                     implements)
         (for-spaces (rhombus/class_clause
                      rhombus/interface_clause
                      rhombus/veneer_clause)
                     extends
                     method
                     property
                     override
                     private
                     protected
                     final)
         (for-spaces (rhombus/class_clause
                      rhombus/interface_clause)
                     internal
                     abstract
                     primitive_property)
         (for-space rhombus/veneer_clause
                    converter))

(define-for-syntax (parse-multiple-names stx)
  (define lines
    (syntax-parse stx
      #:datum-literals (group)
      [(_ (tag::block (group form ...) ...))
       (syntax->list #'((form ...) ...))]
      [(_ form ...)
       (list #'(form ...))]))
  (apply append
         (for/list ([line (in-list lines)])
           (let loop ([line line])
             (syntax-parse line
               [() null]
               [(~var id (:hier-name-seq in-name-root-space in-class-desc-space name-path-op name-root-ref))
                (cons #'id.name (loop #'id.tail))])))))

(define-for-syntax parse-class-extends
  (lambda (stx data)
    (define names (parse-multiple-names stx))
    (wrap-class-clause #`(#:extends . #,names))))

(define-class-clause-syntax extends
  (class-clause-transformer parse-class-extends))

(define-interface-clause-syntax extends
  (interface-clause-transformer parse-class-extends))

(define-veneer-clause-syntax extends
  (veneer-clause-transformer parse-class-extends))

(define-for-syntax parse-class-implements
  (lambda (stx data)
    (define names (parse-multiple-names stx))
    (wrap-class-clause #`(#:implements . #,names))))

(define-class-clause-syntax implements
  (class-clause-transformer parse-class-implements))

(define-veneer-clause-syntax implements
  (veneer-clause-transformer parse-class-implements))

(define-for-syntax parse-class-internal
  (lambda (stx data)
    (syntax-parse stx
      #:datum-literals (group)
      [(_ name:identifier)
       (wrap-class-clause #'(#:internal name))]
      [(_ (tag::block (group name:identifier)))
       (wrap-class-clause #'(#:internal name))]
      [(_ (~and b (tag::block (group name:identifier) ...)))
       (raise-syntax-error #f
                           "multiple ids not allowed"
                           stx
                           #'b)])))

(define-class-clause-syntax internal
  (class-clause-transformer parse-class-internal))

(define-interface-clause-syntax internal
  (interface-clause-transformer parse-class-internal))

(define-class-clause-syntax nonfinal
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(#:nonfinal))]))))

(define-class-clause-syntax opaque
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(#:opaque))]))))

(define-class-clause-syntax authentic
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(#:authentic))]))))

(define-class-clause-syntax prefab
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(#:prefab))]))))

(begin-for-syntax
  (define-splicing-syntax-class (:field-spec mode mutability)
    #:description "field identifier with optional annotation"
    #:attributes (form)
    (pattern (~seq form-id d::var-decl)
             #:with (id:identifier (~optional c::unparsed-inline-annotation)) #'(d.bind ...)
             #:with ann-seq #'(~? c.seq #f)
             #:with form (wrap-class-clause #`(#:field #,mutability id
                                               tmp-id ann-seq d.default form-id
                                               #,mode))))

  (define-syntax-rule (define-clause-form-syntax-class id form-id desc)
    (define-syntax-class id
      #:attributes (name)
      #:description desc
      #:opaque
      (pattern ::name
               #:when (free-identifier=? (in-class-clause-space #'name)
                                         (class-clause-quote form-id)))))
  (define-clause-form-syntax-class :field field "the literal `field`")
  (define-clause-form-syntax-class :immutable immutable "the literal `immutable`")
  (define-clause-form-syntax-class :method method "the literal `method`")
  (define-clause-form-syntax-class :property property "the literal `property`")
  (define-clause-form-syntax-class :override override "the literal `override`")
  (define-clause-form-syntax-class :protected protected "the literal `protected`")
  (define-clause-form-syntax-class :implements implements "the literal `implements`"))

(define-class-clause-syntax field
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [((~var f (:field-spec 'public 'mutable)))
        #'f.form]))))

(define-class-clause-syntax immutable
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~and (~seq _::field _ ...) (~var f (:field-spec 'public 'immutable)))) #'f.form]
       [((~var f (:field-spec 'public 'immutable)))
        #'f.form]))))

(begin-for-syntax
  (define (forwarding-annotations main-ret-stx rets-stx)
    (syntax-parse main-ret-stx
      [()
       (for/list ([ret (syntax->list rets-stx)])
         (syntax-parse ret
           #:datum-literals (group)
           [(op::annotate-op (~optional _::values-id-annot) (~and p (_::parens (~and g (group ret-seq ...)) ...)))
            #:when (attribute op.check?)
            #:with (id ...) (map relocate+reraw
                                 (syntax->list #'(g ...))
                                 (generate-temporaries #'(g ...)))
            #`((id ...) (op (parens (group id) ...)) ((op ret-seq ...) ...))]
           [(op::annotate-op . tail)
            #:when (attribute op.check?)
            #:with id (relocate+reraw #'tail
                                      (car (generate-temporaries '(result-ann))))
            #`((id) (op id) (#,ret))]
           [_
            #`((#f) () (#,ret))]))]
      [_
       (for/list ([ret (syntax->list rets-stx)])
         #`(() #,ret ()))]))

  (define-syntax-class :method-options
    #:attributes (body doc)
    #:datum-literals (group)
    (pattern (b-tag::block
              (~and doc (group #:doc . _))
              g ...)
             #:with body #'(b-tag g ...))
    (pattern (b-tag::block
              (~and name-g (group #:name . _))
              (~and doc (group #:doc . _))
              g ...)
             #:with body #'(b-tag name-g g ...))
    (pattern body
             #:with doc #'#f))

  (define (extract-prefix stx tail-stx)
    (let ([all (syntax->list stx)]
          [tail (syntax->list tail-stx)])
      (reverse (list-tail (reverse all) (length tail)))))

  (define-splicing-syntax-class :maybe-ret
    #:description "optional result annotation"
    #:attributes (seq)
    (pattern (~seq op::annotate-op ret::not-block ...)
             #:with seq #'(op ret ...))
    (pattern (~seq)
             #:with seq #'()))
  (define-splicing-syntax-class (:method-impl stx mode)
    #:description "method implementation"
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq id:identifier (~and args (_::parens . _)) ret::maybe-ret
                   (~and rhs (_::block . _)))
             #:with adj-rhs::method-options #'rhs
             #:with form (wrap-class-clause #`(#,mode id
                                               (block (group fun args adj-rhs.body))
                                               #f
                                               [args ret.seq]
                                               #,(if (syntax-e #'adj-rhs.doc)
                                                     (with-syntax ([(head ...) (extract-prefix stx #'(id args (~@ . ret) block))])
                                                       #`[(head ...)
                                                          (id)
                                                          (args (~@ . ret))
                                                          adj-rhs.doc
                                                          #,stx])
                                                     #'#f))))
    (pattern (~seq (~optional main-id:identifier) main-ret::maybe-ret
                   (~and alts
                         (atag::alts
                          (btag::block ((~and gtag group) a-id:identifier
                                                          (~and args (_::parens . _)) ret::maybe-ret
                                                          (~and body (_::block . _))))
                          ...+)))
             #:do [(when (not (attribute main-id))
                     (syntax-parse #'main-ret.seq
                       [() (void)]
                       [_ (raise-syntax-error #f
                                              "method name required before result annotation"
                                              stx
                                              (respan #'main-ret.seq))]))
                   (define a-ids (syntax->list #'(a-id ...)))
                   (check-consistent stx
                                     (if (attribute main-id) (cons #'main-id a-ids) a-ids)
                                     "name")]
             #:with id (car a-ids)
             #:with ([forwarding-annot-id forwarding-ret forwarded-ret] ...) (forwarding-annotations #'main-ret.seq #'(ret ...))
             #:with form (wrap-class-clause #`(#,mode id
                                               (block (group fun (atag (btag (gtag args (~@ . forwarding-ret) body)) ...)))
                                               #,(if (null? (syntax-e #'main-ret.seq))
                                                     #`([forwarding-annot-id args forwarded-ret] ...)
                                                     #f)
                                               [(parens) main-ret.seq]
                                               #f)))
    (pattern (~seq id:identifier ret::maybe-ret (~and rhs (_::block . _)))
             #:with form (wrap-class-clause #`(#,mode id rhs #f [(parens) ret.seq] #f))))
  (define-splicing-syntax-class (:method-decl stx mode)
    #:description "method declaration"
    #:attributes (id rhs maybe-ret doc forwards)
    #:datum-literals (group)
    (pattern (~seq id:identifier (~and args (tag::parens arg ...)) ret::maybe-ret
                   (_::block (~and kw-g (group #:doc . _))))
             #:with rhs #'(block (group fun (tag arg ...)
                                        (block (group (parsed #:rhombus/expr (void))))))
             #:with maybe-ret #'[args ret.seq]
             #:with doc (syntax-parse stx
                          [(head ... (_::block . _))
                           #`[(group head ...)
                              kw-g
                              #,stx]])
             #:with forwards #f)
    (pattern (~seq _ ... (~and b (_::block . _) (~not (_::block (group #:doc . _)))))
             #:do [(raise-syntax-error #f
                                       "implementation block not allowed"
                                       stx
                                       #'b)]
             #:with id #f
             #:with rhs #f
             #:with maybe-ret #'[(parens) ()]
             #:with doc #'#f
             #:with forwards #f)
    (pattern (~seq id:identifier (~and args (tag::parens arg ...)) ret::maybe-ret)
             #:with rhs #'(block (group fun (tag arg ...)
                                        (block (group (parsed #:rhombus/expr (void))))))
             #:with maybe-ret #'[args ret.seq]
             #:with doc #'#f
             #:with forwards #f)
    (pattern (~seq id:identifier ret::maybe-ret)
             #:with rhs #'#f
             #:with maybe-ret #'[(parens) ret.seq]
             #:with doc #'#f
             #:with forwards #f)
    (pattern (~seq (~optional main-id:identifier) ret::maybe-ret
                   (~and alts
                         (atag::alts
                          (btag::block ((~and gtag group) a-id:identifier
                                                          (~and args (_::parens . _)) case-ret::maybe-ret))
                          ...+)))
             #:do [(when (not (attribute main-id))
                     (syntax-parse #'ret.seq
                       [() (void)]
                       [_ (raise-syntax-error #f
                                              "method name required before result annotation"
                                              stx
                                              (respan #'ret.seq))]))
                   (define a-ids (syntax->list #'(a-id ...)))
                   (check-consistent stx
                                     (if (attribute main-id) (cons #'main-id a-ids) a-ids)
                                     "name")]
             #:with id (car a-ids)
             #:with maybe-ret #'[(parens) ret.seq]
             #:with ([forwarding-annot-id forwarding-ret forwarded-ret] ...) (forwarding-annotations #'ret.seq #'(case-ret ...))
             #:with rhs #'(block (group fun
                                        (atag (btag (gtag args (~@ . forwarding-ret) (block (group (parsed #:rhombus/expr (void))))))
                                              ...)))
             #:with doc #'#f
             #:with forwards (if (null? (syntax-e #'ret.seq))
                                 #'([forwarding-annot-id args forwarded-ret] ...)
                                 #'#f)))
  (define-splicing-syntax-class (:property-impl stx mode)
    #:description "property implementation"
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq id:identifier ret::maybe-ret
                   (~and rhs (_::block . _)))
             #:with adj-rhs::method-options #'rhs
             #:with form (wrap-class-clause #`(#,mode id
                                               (block
                                                (group fun/read-only-property
                                                       (alts
                                                        (block (group (parens) adj-rhs.body))
                                                        (block (group (parens (group ignored))
                                                                      (block (group (parsed #:rhombus/expr (not-assignable 'id)))))))))
                                               #f
                                               [(parens) ret.seq]
                                               #,(if (syntax-e #'adj-rhs.doc)
                                                     (with-syntax ([(head ...) (extract-prefix stx #'(id (~@ . ret) a-block))])
                                                       #`[(head ...)
                                                          (id)
                                                          ret
                                                          adj-rhs.doc
                                                          #,stx])
                                                     #'#f))))

    (pattern (~seq (_::alts
                    (_::block
                     (group id:identifier ret::maybe-ret
                            (~and rhs (_::block . _))))))
             #:with form (wrap-class-clause #`(#,mode id
                                               (block
                                                (group fun/read-only-property
                                                       (alts
                                                        (block (group (parens) rhs))
                                                        (block (group (parens (group ignored))
                                                                      (block (group (parsed #:rhombus/expr (not-assignable 'id)))))))))
                                               #f
                                               [(parens) ret.seq]
                                               #f)))
    (pattern (~seq (~and alts
                         (atag::alts
                          (btag1::block
                           ((~and gtag1 group) a-id1:identifier ret1::maybe-ret
                                               (~and body1 (_::block . _))))
                          (btag2::block
                           ((~and gtag2 group) a-id2:identifier
                                               assign:::=-expr
                                               assign-rhs ...+
                                               (~and body2 (_::block . _)))))))
             #:with adj-body1::method-options #'body1
             #:do [(check-consistent #:who mode #'alts (list #'a-id1 #'a-id2) "name")]
             #:with form (wrap-class-clause #`(#,mode a-id1
                                               (block (group fun
                                                             (atag
                                                              (btag1 (group (parens) adj-body1.body))
                                                              (btag2 (group (parens (group assign-rhs ...))
                                                                            body2)))))
                                               #f
                                               [(parens) ret1.seq]
                                               #,(if (syntax-e #'adj-body1.doc)
                                                     (with-syntax ([(head ...) (extract-prefix stx #'(an-alts))])
                                                       #`[(head ...)
                                                          (a-id1)
                                                          ret1
                                                          adj-body1.doc
                                                          #,stx])
                                                     #'#f)))))
  (define-splicing-syntax-class (:property-decl stx)
    #:description "proper declaration"
    #:attributes (id rhs maybe-ret doc)
    #:datum-literals (group)
    (pattern (~seq id:identifier ret::maybe-ret)
             #:with rhs #'(block (group fun (alts (block (group (parens) (block (group (parsed #:rhombus/expr (void))))))
                                                  (block (group (parens (group _)) (block (group (parsed #:rhombus/expr (void)))))))))
             #:with maybe-ret #'[(parens) ret.seq]
             #:with doc #'#f)
    (pattern (~seq id:identifier ret::maybe-ret
                   (_::block (~and kw-g (group #:doc . _))))
             #:with rhs #'(block (group fun (alts (block (group (parens) (block (group (parsed #:rhombus/expr (void))))))
                                                  (block (group (parens (group _)) (block (group (parsed #:rhombus/expr (void)))))))))
             #:with maybe-ret #'[(parens) ret.seq]
             #:with doc (with-syntax ([(head ...) (extract-prefix stx #'(id (~@ . ret) a-block))])
                          #`[(head ...)
                             (id)
                             ret
                             adj-body1.doc
                             #,stx]))
    (pattern (~seq (_::alts (_::block (group id:identifier ret::maybe-ret))))
             #:with rhs #'(block (group fun (parens) (block (group (parsed #:rhombus/expr (void))))))
             #:with maybe-ret #'[(parens) ret.seq]
             #:with doc #'#f)))

(define-class-clause-syntax constructor
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:none)
        (wrap-class-clause #`(#:constructor #f #f #:none))]
       [(_ (_::block (group #:none)))
        (wrap-class-clause #`(#:constructor #f #f #:none))]
       [(_ #:error)
        (wrap-class-clause #`(#:constructor #f #f #:error))]
       [(_ (_::block (group #:error)))
        (wrap-class-clause #`(#:constructor #f #f #:error))]
       [(_ id:identifier (~and args (_::parens . _)) ret::maybe-ret
           (~and rhs (_::block . _)))
        #:with ([forwarding-annot-id forwarding-ret forwarded-ret]) (forwarding-annotations #'() #'(ret.seq))
        (wrap-class-clause #`(#:constructor id
                              ([forwarding-annot-id args forwarded-ret])
                              (block (group fun args (~@ . forwarding-ret) rhs))))]
       [(_ (~and args (_::parens . _)) ret::maybe-ret
           (~and rhs (_::block . _)))
        #:with ([forwarding-annot-id forwarding-ret forwarded-ret]) (forwarding-annotations #'() #'(ret.seq))
        (wrap-class-clause #`(#:constructor #f
                              ([forwarding-annot-id args forwarded-ret])
                              (block (group fun args (~@ . forwarding-ret) rhs))))]
       [(_ (atag::alts
            (btag::block id:identifier (gtag (~and args (_::parens . _)) ret::maybe-ret
                                             (~and body (_::block . _))))
            ...+))
        #:with ([forwarding-annot-id forwarding-ret forwarded-ret] ...) (forwarding-annotations #'() #'(ret.seq ...))
        #:with (id0 idx ...) #'(id ...)
        (for ([idx (in-list (syntax->list #'(idx ...)))])
          (unless (bound-identifier=? idx #'id0)
            (raise-syntax-error #f "inconsistent name identifier" stx idx)))
        (wrap-class-clause #`(#:constructor id0
                              ([forwarding-annot-id args forwarded-ret] ...)
                              (block (group fun (atag
                                                 (btag id (gtag args (~@ . forwarding-ret)
                                                                body))
                                                 ...)))))]
       [(_ (atag::alts
            (btag::block (gtag (~and args (_::parens . _)) ret::maybe-ret
                               (~and body (_::block . _))))
            ...+))
        #:with ([forwarding-annot-id forwarding-ret forwarded-ret] ...) (forwarding-annotations #'() #'(ret.seq ...))
        (wrap-class-clause #`(#:constructor #f
                              ([forwarding-annot-id args forwarded-ret] ...)
                              (block (group fun (atag
                                                 (btag (gtag args (~@ . forwarding-ret)
                                                             body))
                                                 ...)))))]
       [(_ (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:constructor #f #f rhs))]))))

(define-class-clause-syntax reconstructor
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ #:none)
        (wrap-class-clause #`(#:reconstructor #:none))]
       [(_ (_::block (group #:none)))
        (wrap-class-clause #`(#:reconstructor #:none))]
       [(_ (~and args (_::parens . _)) ret ...
           (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:reconstructor (block (group fun args ret ... rhs))))]
       [(_ (~and rhs (_::alts
                      (_::block (group (_::parens . _) ret ...
                                       (_::block . _)))
                      ...+)))
        (wrap-class-clause #`(#:reconstructor (block (group fun rhs))))]
       [(_ (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:reconstructor rhs))]))))

(define-class-clause-syntax reconstructor_fields
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (_::block (group name:identifier
                                  (~and rhs (_::block . _)))
                           ...))
        (wrap-class-clause #`(#:reconstructor_fields
                              form-id
                              (name ...)
                              ((group fun (parens) rhs) ...)))]))))

(define-class-clause-syntax serializable
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id) (wrap-class-clause
                   #`(#:serializable form-id 0 #f #f #f #f))]
       [(form-id (_::block (~alt
                            (~optional (~or (group #:version (_::block (group version)))
                                            (group #:version version))
                                       #:defaults ([version #'0]))
                            (~optional (group #:serialize (~and (_::block . _) s-rhs))
                                       #:defaults ([s-rhs #'#f]))
                            (~optional (group #:deserialize (~and (_::block . _) d-rhs))
                                       #:defaults ([d-rhs #'#f]))
                            (~optional (group (~and ds #:deserialize_shell) (~and (_::block . _) ds-rhs))
                                       #:defaults ([ds-rhs #'#f]
                                                   [ds #'#f]))
                            (~optional (group (~and df #:deserialize_fill) (~and (_::block . _) df-rhs))
                                       #:defaults ([df-rhs #'#f]
                                                   [df #'#f])))
                           ...))
        (unless (exact-nonnegative-integer? (syntax-e #'version))
          (raise-syntax-error #f "need an exact nonnegative integer for version" stx #'version))
        (when (syntax-e #'ds)
          (when (not (syntax-e #'d-rhs))
            (raise-syntax-error #f "need custom deserialize to go with deserialize shell" stx #'ds))
          (when (not (syntax-e #'df))
            (raise-syntax-error #f "need deserialize fill to go with deserialize shell" stx #'ds)))
        (when (and (syntax-e #'df) (not (syntax-e #'ds)))
          (raise-syntax-error #f "need deserialize shell to go with deserialize fill" stx #'df))
        (wrap-class-clause
         #`(#:serializable form-id version s-rhs d-rhs ds-rhs df-rhs))]))))

(define-for-syntax (parse-final stx data)
  (syntax-parse stx
    [(_ _::override _::method (~var m (:method-impl stx #'#:final-override))) #'m.form]
    [(_ _::method (~var m (:method-impl stx #'#:final))) #'m.form]
    [(_ _::protected (~var m (:method-impl stx #'#:final-protected))) #'m.form]
    [(_ _::protected _::method (~var m (:method-impl stx #'#:final-protected))) #'m.form]
    [(_ _::override _::property (~var m (:property-impl stx #'#:final-override-property))) #'m.form]
    [(_ _::property (~var m (:property-impl stx #'#:final-property))) #'m.form]
    [(_ _::override (~var m (:method-impl stx #'#:final-override))) #'m.form]
    [(_ _::override _::property (~var m (:property-impl stx #'#:final-override-property))) #'m.form]
    [(_ _::protected _::property (~var m (:property-impl stx #'#:final-protected-property))) #'m.form]
    [(_ (~var m (:method-impl stx #'#:final))) #'m.form]))

(define-class-clause-syntax final
  (class-clause-transformer parse-final))

(define-interface-clause-syntax final
  (interface-clause-transformer parse-final))

;; redundant, but allowed:
(define-veneer-clause-syntax final
  (veneer-clause-transformer parse-final))

(define-veneer-clause-syntax converter
  (veneer-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(#:converter))]))))

(define-for-syntax parse-class-method
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method-impl stx #'#:method))) #'m.form])))

(define-class-clause-syntax method
  (class-clause-transformer parse-class-method))

(define-interface-clause-syntax method
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method-impl stx #'#:method))) #'m.form]
       [(_ (~var decl (:method-decl stx #'#:method))) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]))))

(define-veneer-clause-syntax method
  (veneer-clause-transformer parse-class-method))

(define-for-syntax parse-class-property
  (lambda (stx data)
    (syntax-parse stx
      [(_ (~var m (:property-impl stx #'#:property))) #'m.form])))

(define-class-clause-syntax property
  (class-clause-transformer parse-class-property))

(define-interface-clause-syntax property
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:property-impl stx #'#:property))) #'m.form]
       [(_ (~var decl (:property-decl stx))) (wrap-class-clause #'(#:abstract-property decl.id decl.rhs #f decl.maybe-ret decl.doc))]))))

(define-veneer-clause-syntax property
  (veneer-clause-transformer parse-class-property))

(define-for-syntax parse-class-override
  (lambda (stx data)
    (syntax-parse stx
      [(_ _::method (~var m (:method-impl stx #'#:override))) #'m.form]
      [(_ _::property (~var m (:property-impl stx #'#:override-property))) #'m.form]
      [(_ (~var m (:method-impl stx #'#:override))) #'m.form])))

(define-class-clause-syntax override
  (class-clause-transformer parse-class-override))

(define-interface-clause-syntax override
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ _::method (~var m (:method-impl stx #'#:override))) #'m.form]
       [(_ _::method (~var decl (:method-decl stx #'#:override))) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
       [(_ _::property (~var m (:property-impl stx #'#:override-property))) #'m.form]
       [(_ _::property (~var decl (:property-decl stx))) (wrap-class-clause #'(#:abstract-override-property decl.id decl.rhs #f decl.maybe-ret decl.doc))]
       [(_ (~var m (:method-impl stx #'#:override))) #'m.form]
       [(_ (~var decl (:method-decl stx #'#:override))) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]))))

(define-veneer-clause-syntax override
  (veneer-clause-transformer parse-class-override))

(define-for-syntax parse-class-private
  (lambda (stx data)
    (syntax-parse stx
      [(_ tag::implements form ...)
       (wrap-class-clause #`(#:private-implements . #,(parse-multiple-names #'(tag form ...))))]
      [(_ _::method (~var m (:method-impl stx #'#:private))) #'m.form]
      [(_ _::override _::property (~var m (:property-impl stx #'#:private-override-property))) #'m.form]
      [(_ _::override (~var m (:method-impl stx #'#:private-override))) #'m.form]
      [(_ _::override _::method (~var m (:method-impl stx #'#:private-override))) #'m.form]
      [(_ _::property (~var m (:property-impl stx #'#:private-property))) #'m.form]
      [(_ _::immutable (~and (~seq _::field _ ...) (~var f (:field-spec 'private 'immutable)))) #'f.form]
      [(_ (~and (~seq _::immutable _ ...) (~var f (:field-spec 'private 'immutable)))) #'f.form]
      [(_ (~and (~seq _::field _ ...) (~var f (:field-spec 'private 'mutable)))) #'f.form]
      [(_ (~var m (:method-impl stx #'#:private))) #'m.form])))

(define-class-clause-syntax private
  (class-clause-transformer parse-class-private))

(define-interface-clause-syntax private
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ _::method (~var m (:method-impl stx #'#:private))) #'m.form]
       [(_ (~var m (:method-impl stx #'#:private))) #'m.form]))))

(define-veneer-clause-syntax private
  (veneer-clause-transformer parse-class-private))

(define-for-syntax parse-class-protected
  (lambda (stx data)
    (syntax-parse stx
      [(_ tag::implements form ...)
       (wrap-class-clause #`(#:protected-implements . #,(parse-multiple-names #'(tag form ...))))]
      [(_ _::method (~var m (:method-impl stx #'#:protected))) #'m.form]
      [(_ _::property (~var m (:property-impl stx #'#:protected-property))) #'m.form]
      [(_ _::immutable (~and (~seq _::field _ ...) (~var f (:field-spec 'protected 'mutable)))) #'f.form]
      [(_ (~and (~seq _::field _ ...) (~var f (:field-spec 'protected 'mutable)))) #'f.form]
      [(_ (~and (~seq _::immutable _ ...) (~var f (:field-spec 'protected 'immutable)))) #'f.form]
      [(_ (~var m (:method-impl stx #'#:protected))) #'m.form])))

(define-class-clause-syntax protected
  (class-clause-transformer parse-class-protected))

(define-for-syntax parse-protected
  (lambda (stx data)
    (syntax-parse stx
      [(_ _::method (~var m (:method-impl stx #'#:protected))) #'m.form]
      [(_ _::property (~var m (:property-impl stx #'#:protected-property))) #'m.form]
      [(_ (~var m (:method-impl stx #'#:protected))) #'m.form])))

(define-interface-clause-syntax protected
  (interface-clause-transformer parse-protected))

(define-veneer-clause-syntax protected
  (veneer-clause-transformer parse-protected))

(define-for-syntax (parse-abstract-clause stx data)
  (syntax-parse stx
    [(_ _::method (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
    [(_ _::protected (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract-protected decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
    [(_ _::protected _::method (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract-protected decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
    [(_ _::property (~var decl (:property-decl stx))) (wrap-class-clause #'(#:abstract-property decl.id decl.rhs #f decl.maybe-ret decl.doc))]
    [(_ _::override (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
    [(_ _::override _::method (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]
    [(_ _::override _::property (~var decl (:property-decl stx))) (wrap-class-clause #'(#:abstract-override-property decl.id decl.rhs #f decl.maybe-ret decl.doc))]
    [(_ _::protected _::property (~var decl (:property-decl stx))) (wrap-class-clause #'(#:abstract-protected-property decl.id decl.rhs #f decl.maybe-ret decl.doc))]
    [(_ (~var decl (:method-decl stx #'#:abstract))) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.forwards decl.maybe-ret decl.doc))]))

(define-class-clause-syntax abstract
  (class-clause-transformer parse-abstract-clause))
(define-interface-clause-syntax abstract
  (interface-clause-transformer parse-abstract-clause))

(define-for-syntax parse-class-primitive-property
  (lambda (stx data)
    (syntax-parse stx
      [(form prop_expr ... (b-tag::block val_body ...))
       #`[(group (parsed #:rhombus/defn
                         [(define prop (check-primitive-property
                                        'form
                                        (rhombus-expression (group prop_expr ...))))
                          (define prop-val (rhombus-body-at b-tag val_body ...))]))
          (group primitive_property_finish prop prop-val)]])))

(define (check-primitive-property who v)
  (unless (struct-type-property? v)
    (raise-annotation-failure who v "PrimitiveProperty"))
  v)

(define-class-clause-syntax primitive_property
  (class-clause-transformer parse-class-primitive-property))

(define-interface-clause-syntax primitive_property
  (interface-clause-transformer parse-class-primitive-property))

(define-for-syntax parse-class-primitive-property-finish
  (lambda (stx data)
    (syntax-parse stx
      [(_ prop-id prop-val-id)
       (wrap-class-clause #'(#:primitive-property prop-id prop-val-id))])))

(define-class-clause-syntax primitive_property_finish
  (class-clause-transformer parse-class-primitive-property-finish))

(define-interface-clause-syntax primitive_property_finish
  (interface-clause-transformer parse-class-primitive-property-finish))

(define (not-assignable name)
  (error name "property does not support assignment"))
