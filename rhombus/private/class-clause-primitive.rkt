#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/name-parse
                     "name-path-op.rkt"
                     "class-parse.rkt"
                     "consistent.rkt")
         "provide.rkt"
         "class-clause.rkt"
         "class-clause-tag.rkt"
         (submod "class-clause.rkt" for-class)
         "interface-clause.rkt"
         (submod "annotation.rkt" for-class)
         "parens.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "var-decl.rkt"
         (only-in "function.rkt" fun)
         (submod "function.rkt" for-method)
         (only-in "implicit.rkt" #%body)
         "op-literal.rkt")

(provide (for-space rhombus/class_clause
                    implements
                    nonfinal
                    opaque
                    prefab
                    authentic
                    field
                    constructor
                    reconstructor
                    reconstructor_fields)
         (for-spaces (rhombus/class_clause
                      rhombus/interface_clause)
                     extends
                     internal
                     final
                     method
                     property
                     override
                     private
                     abstract))

(define-for-syntax (parse-multiple-names stx)
  (define lines
    (syntax-parse stx
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

(define-class-clause-syntax extends
  (class-clause-transformer
   (lambda (stx data)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(#:extends . #,names)))))

(define-interface-clause-syntax extends
  (interface-clause-transformer
   (lambda (stx data)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(#:extends . #,names)))))

(define-class-clause-syntax implements
  (class-clause-transformer
   (lambda (stx data)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(#:implements . #,names)))))

(define-class-clause-syntax internal
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(#:internal name))]))))

(define-interface-clause-syntax internal
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(#:internal name))]))))

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
  (define-splicing-syntax-class (:field mode)
    #:description "field identifier with optional annotation"
    #:attributes (form)
    (pattern (~seq form-id d::var-decl)
             #:with (id:identifier (~optional c::unparsed-inline-annotation)) #'(d.bind ...)
             #:with ann-seq (if (attribute c)
                                #'c.seq
                                #'#f)
             #:with form (wrap-class-clause #`(#:field id
                                               tmp-id ann-seq d.blk form-id
                                               #,mode)))))

(define-class-clause-syntax field
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [((~var f (:field 'public)))
        #'f.form]))))

(begin-for-syntax
  (define-splicing-syntax-class :maybe-ret
    #:attributes (seq)
    (pattern (~seq op::annotate-op ret ...)
             #:with seq #'(op ret ...))
    (pattern (~seq)
             #:with seq #'()))
  (define-splicing-syntax-class (:method-impl mode)
    #:description "method implementation"
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq id:identifier (~and args (_::parens . _)) ret::maybe-ret
                   (~and rhs (_::block . _)))
             #:with form (wrap-class-clause #`(#,mode id
                                               (block (group fun args rhs))
                                               ret.seq)))
    (pattern (~seq (~and alts
                         (atag::alts
                          (btag::block ((~and gtag group) a-id:identifier
                                                          (~and args (_::parens . _)) ret::maybe-ret
                                                          (~and body (_::block . _))))
                          ...+)))
             #:do [(define a-ids (syntax->list #'(a-id ...)))
                   (check-consistent #:who mode #'alts a-ids "name")]
             #:with id (car a-ids)
             #:with (ret0 ...) (let ([retss (syntax->list #'(ret.seq ...))])
                                 (if (for/and ([rets (in-list (cdr retss))])
                                       (same-return-signature? (car retss) rets))
                                     (car retss)
                                     '()))
             #:with form (wrap-class-clause #`(#,mode id
                                               (block (group fun (atag (btag (gtag args body)) ...)))
                                               (ret0 ...))))
    (pattern (~seq id:identifier ret::maybe-ret (~and rhs (_::block . _)))
             #:with form (wrap-class-clause #`(#,mode id rhs ret.seq))))
  (define-splicing-syntax-class :method-decl
    #:description "method declaration"
    #:attributes (id rhs maybe-ret)
    (pattern (~seq id:identifier (tag::parens arg ...) ret::maybe-ret)
             #:with rhs #'(block (group fun (tag arg ...)
                                        (block (group (parsed #:rhombus/expr (void))))))
             #:with maybe-ret #'ret.seq)
    (pattern (~seq id:identifier ret::maybe-ret)
             #:with rhs #'#f
             #:with maybe-ret #'ret.seq))
  (define-splicing-syntax-class (:property-impl mode)
    #:description "property implementation"
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq id:identifier ret::maybe-ret
                   (~and rhs (_::block . _)))
             #:with form (wrap-class-clause #`(#,mode id
                                               (block
                                                (group fun/read-only-property
                                                       (alts
                                                        (block (group (parens) rhs))
                                                        (block (group (parens (group ignored))
                                                                      (block (group (parsed #:rhombus/expr (not-assignable 'id)))))))))
                                               ret.seq)))

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
                                               ret.seq)))
    (pattern (~seq (~and alts
                         (atag::alts
                          (btag1::block
                           ((~and gtag1 group) a-id1:identifier ret1::maybe-ret
                                               (~and body1 (_::block . _))))
                          (btag2::block
                           ((~and gtag2 group) a-id2:identifier
                                               _:::=-expr
                                               assign-rhs ...+
                                               (~and body2 (_::block . _)))))))
             #:do [(check-consistent #:who mode #'alts (list #'a-id1 #'a-id2) "name")]
             #:with form (wrap-class-clause #`(#,mode a-id1
                                               (block (group fun
                                                             (atag
                                                              (btag1 (group (parens) body1))
                                                              (btag2 (group (parens (group assign-rhs ...))
                                                                            body2)))))
                                               ret1.seq))))
  (define-splicing-syntax-class :property-decl
    #:description "proper declaration"
    #:attributes (id rhs maybe-ret)
    #:datum-literals (group)
    (pattern (~seq id:identifier ret::maybe-ret)
             #:with rhs #'(block (group fun (alts (block (group (parens) (block (group (parsed #:rhombus/expr (void))))))
                                                  (block (group (parens (group _)) (block (group (parsed #:rhombus/expr (void)))))))))
             #:with maybe-ret #'ret.seq)
    (pattern (~seq (_::alts (_::block (group id:identifier ret::maybe-ret))))
             #:with rhs #'(block (group fun (parens) (block (group (parsed #:rhombus/expr (void))))))
             #:with maybe-ret #'ret.seq)))

(define-class-clause-syntax constructor
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ id:identifier (~and args (_::parens . _)) ret ...
           (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:constructor id (block (group fun args ret ... rhs))))]
       [(_ (~and args (_::parens . _)) ret ...
           (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:constructor #f (block (group fun args ret ... rhs))))]
       [(_ (~and rhs (_::alts
                      (_::block id:identifier (group (_::parens . _) ret ...
                                                     (_::block . _)))
                      ...+)))
        #:with (id0 idx ...) #'(id ...)
        (for ([idx (in-list (syntax->list #'(idx ...)))])
          (unless (bound-identifier=? idx #'id0)
            (raise-syntax-error #f "inconsistent name identifier" stx idx)))
        (wrap-class-clause #`(#:constructor id0 (block (group fun rhs))))]
       [(_ (~and rhs (_::alts
                      (_::block (group (_::parens . _) ret ...
                                       (_::block . _)))
                      ...+)))
        (wrap-class-clause #`(#:constructor #f (block (group fun rhs))))]
       [(_ id:identifier (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:constructor id rhs))]
       [(_ (~and rhs (_::block . _)))
        (wrap-class-clause #`(#:constructor #f rhs))]))))

(define-class-clause-syntax reconstructor
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
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

(begin-for-syntax
  (define-syntax-rule (define-clause-form-syntax-class id form-id desc)
    (define-syntax-class id
      #:attributes (name)
      #:description desc
      #:opaque
      (pattern ::name
               #:when (free-identifier=? (in-class-clause-space #'name)
                                         (class-clause-quote form-id)))))
  (define-clause-form-syntax-class :method method "the literal `method`")
  (define-clause-form-syntax-class :property property "the literal `property`")
  (define-clause-form-syntax-class :override override "the literal `override`")
  (define-clause-form-syntax-class :implements implements "the literal `implements`"))

(define-for-syntax (parse-final stx data)
  (syntax-parse stx
    [(_ _::override _::method (~var m (:method-impl #'#:final-override))) #'m.form]
    [(_ _::method (~var m (:method-impl #'#:final))) #'m.form]
    [(_ _::override _::property (~var m (:property-impl #'#:final-override-property))) #'m.form]
    [(_ _::property (~var m (:property-impl #'#:final-property))) #'m.form]
    [(_ _::override (~var m (:method-impl #'#:final-override))) #'m.form]
    [(_ _::override _::property (~var m (:property-impl #'#:final-override-property))) #'m.form]
    [(_ (~var m (:method-impl #'#:final))) #'m.form]))

(define-class-clause-syntax final
  (class-clause-transformer parse-final))

(define-interface-clause-syntax final
  (interface-clause-transformer parse-final))

(define-class-clause-syntax method
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method-impl #'#:method))) #'m.form]))))

(define-interface-clause-syntax method
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method-impl #'#:method))) #'m.form]
       [(_ decl::method-decl) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.maybe-ret))]))))

(define-class-clause-syntax property
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:property-impl #'#:property))) #'m.form]))))

(define-interface-clause-syntax property
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:property-impl #'#:property))) #'m.form]
       [(_ decl::property-decl) (wrap-class-clause #'(#:abstract-property decl.id decl.rhs decl.maybe-ret))]))))

(define-class-clause-syntax override
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ _::method (~var m (:method-impl #'#:override))) #'m.form]
       [(_ _::property (~var m (:property-impl #'#:override-property))) #'m.form]
       [(_ (~var m (:method-impl #'#:override))) #'m.form]))))

(define-interface-clause-syntax override
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ _::method (~var m (:method-impl #'#:override))) #'m.form]
       [(_ _::method decl::method-decl) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.maybe-ret))]
       [(_ _::property (~var m (:property-impl #'#:override-property))) #'m.form]
       [(_ _::property decl::property-decl) (wrap-class-clause #'(#:abstract-override-property decl.id decl.rhs decl.maybe-ret))]
       [(_ (~var m (:method-impl #'#:override))) #'m.form]
       [(_ decl::method-decl) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.maybe-ret))]))))

(define-class-clause-syntax private
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ tag::implements form ...)
        (wrap-class-clause #`(#:private-implements . #,(parse-multiple-names #'(tag form ...))))]
       [(_ _::method (~var m (:method-impl #'#:private))) #'m.form]
       [(_ _::override _::property (~var m (:property-impl #'#:private-override-property))) #'m.form]
       [(_ _::override (~var m (:method-impl #'#:private-override))) #'m.form]
       [(_ _::override method (~var m (:method-impl #'#:private-override))) #'m.form]
       [(_ _::property (~var m (:property-impl #'#:private-property))) #'m.form]
       [(_ (~and (~seq field _ ...) (~var f (:field 'private)))) #'f.form]
       [(_ (~var m (:method-impl #'#:private))) #'m.form]))))

(define-interface-clause-syntax private
  (interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ _::method (~var m (:method-impl #'#:private))) #'m.form]
       [(_ (~var m (:method-impl #'#:private))) #'m.form]))))

(define-for-syntax (parse-abstract-clause stx data)
  (syntax-parse stx
    [(_ _::method decl::method-decl) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.maybe-ret))]
    [(_ _::property decl::property-decl) (wrap-class-clause #'(#:abstract-property decl.id decl.rhs decl.maybe-ret))]
    [(_ _::override decl::method-decl) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.maybe-ret))]
    [(_ _::override _::method decl::method-decl) (wrap-class-clause #'(#:abstract-override decl.id decl.rhs decl.maybe-ret))]
    [(_ _::override _::property decl::property-decl) (wrap-class-clause #'(#:abstract-override-property decl.id decl.rhs decl.maybe-ret))]
    [(_ decl::method-decl) (wrap-class-clause #'(#:abstract decl.id decl.rhs decl.maybe-ret))]))

(define-class-clause-syntax abstract
  (class-clause-transformer parse-abstract-clause))
(define-interface-clause-syntax abstract
  (interface-clause-transformer parse-abstract-clause))

(define-for-syntax (same-return-signature? a b)
  (cond
    [(identifier? a)
     (and (identifier? b)
          ;; This is stronger than we'd like, but
          ;; `free-identifier=?` is too weak, because it doesn't
          ;; check all spaces
          (bound-identifier=? a b))]
    [(identifier? b) #f]
    [(syntax? a)
     (same-return-signature? (syntax-e a) b)]
    [(syntax? b)
     (same-return-signature? a (syntax-e b))]
    [(null? a) (null? b)]
    [(pair? a)
     (and (pair? b)
          (and (same-return-signature? (car a) (car b))
               (same-return-signature? (cdr a) (cdr b))))]
    [else (equal? a b)]))

(define (not-assignable name)
  (error name "property does not support assignment"))
