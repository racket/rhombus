#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "class-parse.rkt"
                     (only-in "rule.rkt" rule)
                     "consistent.rkt"
                     "class-data.rkt")
         "class+interface.rkt"
         "class-clause.rkt"
         "interface-clause.rkt"
         (only-in "annotation.rkt" :: -:)
         (submod "annotation.rkt" for-class)
         "parens.rkt"
         "name-root-ref.rkt"
         "parse.rkt"
         "var-decl.rkt"
         (only-in "function.rkt" fun)
         (only-in "implicit.rkt" #%body))

(provide (for-syntax extract-internal-ids
                     parse-annotation-options
                     parse-options
                     wrap-class-clause)
         rhombus-class
         extends
         implements
         internal
         constructor
         binding
         annotation
         final
         nonfinal
         authentic
         field
         method
         property
         override
         private
         abstract)

(module+ for-interface
  (provide (for-syntax parse-method-clause)))

(define-for-syntax (extract-internal-ids options
                                         scope-stx base-stx
                                         stxes)
  (define internal-id (hash-ref options 'internal #f))
  (define expose (if internal-id
                     (let ([intro (make-syntax-delta-introducer scope-stx base-stx)])
                       (lambda (stx)
                         (intro stx 'remove)))
                     (lambda (stx) stx)))
  (values internal-id
          (expose internal-id)))

(define-for-syntax (extract-rhs b)
  (syntax-parse b
    [(_::block g) #'g]
    [else
     (raise-syntax-error #f
                         "expected a single entry point in block body"
                         b)]))

(define-for-syntax (parse-annotation-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (let loop ([clauses (syntax->list #'(clause-parsed ...))] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends implements private-implements
                                  constructor final nonfinal authentic binding annotation
                                  method property private override abstract internal
                                  final-override private-override
                                  override-property final-property final-override-property
                                  private-property private-override-property
                                  abstract-property)
              [(extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "multiple extension clauses" orig-stx clause))
               (hash-set options 'extends #'id)]
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "multiple internal-name clauses" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(annotation block)
               (when (hash-has-key? options 'annotation-rhs)
                 (raise-syntax-error #f "multiple annotation clauses" orig-stx clause))
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [_ options]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (define (add-implements options extra-key ids-stx)
       (define l (reverse (syntax->list ids-stx)))
       (define new-options
         (hash-set options 'implements (append l (hash-ref options 'implements '()))))
       (if extra-key
           (hash-set new-options extra-key (append l (hash-ref new-options extra-key '())))
           new-options))
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends implements private-implements
                                  constructor final nonfinal authentic binding annotation
                                  method property private override abstract internal
                                  final-override private-override
                                  override-property final-property final-override-property
                                  private-property private-override-property
                                  abstract-property)
              [(extends id) ; checked in `parse-annotation-options`
               (hash-set options 'extends #'id)]
              [(implements id ...)
               (add-implements options 'public-implements #'(id ...))]
              [(private-implements id ...)
               (add-implements options 'private-implements #'(id ...))]
              [(internal id) ; checked in `parse-annotation-options`
               (hash-set options 'internal #'id)]
              [(constructor rhs)
               (when (hash-has-key? options 'constructor-rhs)
                 (raise-syntax-error #f "multiple constructor clauses" orig-stx clause))
               (hash-set options 'constructor-rhs #'rhs)]
              [(binding block)
               (when (hash-has-key? options 'binding-rhs)
                 (raise-syntax-error #f "multiple binding clauses" orig-stx clause))
               (hash-set options 'binding-rhs (extract-rhs #'block))]
              [(annotation block) ; checked in `parse-annotation-options`
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [(nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "multiple finality clauses" orig-stx clause))
               (hash-set options 'final? #f)]
              [(authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "multiple authenticity clause" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(field id rhs-id ann-seq blk form-id mode)
               (with-syntax ([(predicate annotation-str static-infos)
                              (syntax-parse #'ann-seq
                                [#f (list #'#f #'#f #'())]
                                [(c::inline-annotation)
                                 (list #'c.predicate #'c.annotation-str #'c.static-infos)])])
                 (hash-set options 'fields (cons (added-field #'id
                                                              #'rhs-id #'blk #'form-id
                                                              #'static-infos
                                                              #'predicate
                                                              #'annotation-str
                                                              (syntax-e #'mode))
                                                 (hash-ref options 'fields null))))]
              [_
               (parse-method-clause orig-stx options clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (parse-method-clause orig-stx options clause)
  (syntax-parse clause
    #:literals (extends implements private-implements
                        constructor final nonfinal authentic binding annotation
                        method property private override abstract internal
                        final-override private-override
                        override-property final-property final-override-property
                        private-property private-override-property
                        abstract-property
                        abstract-override abstract-override-property)
    [((~and tag (~or method override private final final-override private-override
                     property override-property
                     final-property final-override-property
                     private-property private-override-property))
      id rhs maybe-ret)
     (define-values (body replace disposition kind)
       (case (syntax-e #'tag)
         [(method) (values 'method 'method 'abstract 'method)]
         [(override) (values 'method 'override 'abstract 'method)]
         [(private) (values 'method 'method 'private 'method)]
         [(private-override) (values 'method 'override 'private 'method)]
         [(final) (values 'method 'method 'final 'method)]
         [(final-override) (values 'method 'override 'final 'method)]
         [(property) (values 'method 'method 'abstract 'property)]
         [(override-property) (values 'method 'override 'abstract 'property)]
         [(final-property) (values 'method 'method 'final 'property)]
         [(final-override-property) (values 'method 'override 'final 'property)]
         [(private-property) (values 'method 'method 'private 'property)]
         [(private-override-property) (values 'method 'override 'private 'property)]
         [else (error "method kind not handled" #'tag)]))
     (hash-set options 'methods (cons (added-method #'id
                                                    (car (generate-temporaries #'(id)))
                                                    #'rhs
                                                    #'maybe-ret
                                                    (and (pair? (syntax-e #'maybe-ret))
                                                         (car (generate-temporaries #'(id))))
                                                    body
                                                    replace
                                                    disposition
                                                    kind)
                                      (hash-ref options 'methods null)))]
    [((~and tag (~or abstract abstract-property abstract-override abstract-override-property))
      id rhs maybe-ret)
     (define-values (replace kind)
       (case (syntax-e #'tag)
         [(abstract) (values 'method 'method)]
         [(abstract-property) (values 'method 'property)]
         [(abstract-override) (values 'override 'method)]
         [(abstract-override-property) (values 'override 'property)]
         [else (error "method kind not handled" #'tag)]))
     (hash-set options 'methods (cons (added-method #'id
                                                    '#:abstract
                                                    #'rhs
                                                    #'maybe-ret
                                                    (and (pair? (syntax-e #'maybe-ret))
                                                         (car (generate-temporaries #'(id))))
                                                    'abstract
                                                    replace
                                                    'abstract
                                                    kind)
                                      (hash-ref options 'methods null)))]
    [_
     (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))

(define-syntax rhombus-class 'placeholder)

(define-for-syntax (wrap-class-clause parsed)
  #`[(group (parsed (quote-syntax (rhombus-class #,parsed) #:local)))]) ; `quote-syntax` + `rhombus-class` wrapper => clause

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
               [(~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref))
                (cons #'id.name (loop #'id.tail))])))))

(define-syntax extends
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~seq form ...))
        #:with (~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref)) #'(form ...)
        #:with () #'id.tail
        (wrap-class-clause #'(extends id.name))]))
   ;; interface clause
   (lambda (stx data)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(extends . #,names)))))

(define-syntax implements
  (class-clause-transformer
   (lambda (stx data)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(implements . #,names)))))

(define-syntax internal
  (make-class+interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(internal name))]))))

(define-syntax binding
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (~and (_::quotes . _)
                         pattern)
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(binding (block (named-rule rule #,stx pattern template-block))))]
       [(form-name (~and rhs (_::alts
                              (_::block (group (_::quotes . _)
                                               (_::block . _)))
                              ...)))
        (wrap-class-clause #`(binding (block (named-rule rule #,stx rhs))))]
       [(form-name (~and (_::block . _)
                         binding-block))
        (wrap-class-clause #`(binding binding-block))]))))

(define-syntax annotation
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (~and (_::quotes . _)
                         pattern)
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(annotation (block (named-rule rule #,stx pattern template-block))))]
       [(form-name (~and rhs (_::alts
                              (_::block (group (_::quotes . _)
                                               (_::block . _)))
                              ...)))
        (wrap-class-clause #`(annotation (block (named-rule rule #,stx rhs))))]
       [(form-name (~and (_::block . _)
                         annotation-block))
        (wrap-class-clause #`(annotation annotation-block))]))))

(define-syntax nonfinal
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(nonfinal))]))))

(define-syntax authentic
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(authentic))]))))

(begin-for-syntax
  (define-splicing-syntax-class (:field mode)
    #:description "field identifier with optional annotation"
    #:attributes (form)
    (pattern (~seq form-id d::var-decl)
             #:with (id:identifier (~optional c::unparsed-inline-annotation)) #'(d.bind ...)
             #:attr ann-seq (if (attribute c)
                                #'c.seq
                                #'#f)
             #:attr form (wrap-class-clause #`(field id
                                                     tmp-id ann-seq d.blk form-id
                                                     #,mode)))))

(define-syntax field
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       [((~var f (:field 'public)))
        #'f.form]))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

(begin-for-syntax
  (define-splicing-syntax-class :maybe-ret
    #:attributes (seq)
    #:literals (:: -:)
    #:datum-literals (op)
    (pattern (~seq (~and o (op (~or :: -:))) ret ...)
             #:attr seq #'(o ret ...))
    (pattern (~seq)
             #:attr seq #'()))
  (define-splicing-syntax-class (:method mode)
    #:description "method implementation"
    #:attributes (form)
    #:datum-literals (group)
    (pattern (~seq id:identifier (~and args (_::parens . _)) ret::maybe-ret
                   (~and rhs (_::block . _)))
             #:attr form (wrap-class-clause #`(#,mode id
                                               (block (group fun args rhs))
                                               ret.seq)))
    (pattern (~seq (~and alts
                         (atag::alts
                          (btag::block ((~and gtag group) a-id:identifier
                                                          (~and args (_::parens . _)) ret::maybe-ret
                                                          (~and body (_::block . _))))
                          ...+)))
             #:do [(define a-ids (syntax->list #'(a-id ...)))
                   (check-consistent #:who (syntax-e mode) #'alts a-ids "name")]
             #:attr id (car a-ids)
             #:with (ret0 ...) (let ([retss (syntax->list #'(ret.seq ...))])
                                 (if (for/and ([rets (in-list (cdr retss))])
                                       (same-return-signature? (car retss) rets))
                                     (car retss)
                                     '()))
             #:attr form (wrap-class-clause #`(#,mode id
                                               (block (group fun (atag (btag (gtag args body)) ...)))
                                               (ret0 ...))))
    (pattern (~seq id:identifier ret::maybe-ret (~and rhs (_::block . _)))
             #:attr form (wrap-class-clause #`(#,mode id rhs ret.seq))))
  (define-splicing-syntax-class :method-decl
    #:description "method declaration"
    #:attributes (id rhs maybe-ret)
    (pattern (~seq id:identifier (tag::parens arg ...) ret::maybe-ret)
             #:attr rhs #'(group fun (tag arg ...)
                                 (block (group (parsed (void)))))
             #:attr maybe-ret #'ret.seq)
    (pattern (~seq id:identifier ret::maybe-ret)
             #:attr rhs #'#f
             #:attr maybe-ret #'ret.seq)))

(define-syntax constructor
  (class-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and args (_::parens . _)) ret ...
           (~and rhs (_::block . _)))
        (wrap-class-clause #`(constructor (block (group fun args ret ... rhs))))]
       [(_ (~and rhs (_::alts
                      (_::block (group (_::parens . _) ret ...
                                       (_::block . _)))
                      ...+)))
        (wrap-class-clause #`(constructor (block (group fun rhs))))]
       [(_ (~and rhs (_::block . _)))
        (wrap-class-clause #`(constructor rhs))]))))

(define-syntax final
  (make-class+interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:literals (override method property)
       [(_ override method (~var m (:method #'final-override))) #'m.form]
       [(_ method (~var m (:method #'final))) #'m.form]
       [(_ override property (~var m (:method #'final-override-property))) #'m.form]
       [(_ property (~var m (:method #'final-property))) #'m.form]
       [(_ override (~var m (:method #'final-override))) #'m.form]
       [(_ override property (~var m (:method #'final-override-property))) #'m.form]
       [(_ (~var m (:method #'final))) #'m.form]))))
(define-syntax final-override 'placeholder)
(define-syntax final-property 'placeholder)
(define-syntax final-override-property 'placeholder)

(define-syntax method
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method #'method))) #'m.form]))
   ;; interface clause
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method #'method))) #'m.form]
       [(_ decl::method-decl) (wrap-class-clause #'(abstract decl.id decl.rhs decl.maybe-ret))]))))

(define-syntax property
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method #'property))) #'m.form]))
   ;; interface clause
   (lambda (stx data)
     (syntax-parse stx
       [(_ (~var m (:method #'property))) #'m.form]
       [(_ decl::method-decl) (wrap-class-clause #'(abstract-property decl.id decl.rhs decl.maybe-ret))]))))

(define-syntax override
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx data)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'override))) #'m.form]
       [(_ property (~var m (:method #'override-property))) #'m.form]
       [(_ (~var m (:method #'override))) #'m.form]))
   (lambda (stx data)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'override))) #'m.form]
       [(_ property (~var m (:method #'override-property))) #'m.form]
       [(_ (~var m (:method #'override))) #'m.form]
       [(_ decl::method-decl) (wrap-class-clause #'(abstract-override decl.id decl.rhs decl.maybe-ret))]))))
(define-syntax override-property 'placeholder)

(define-syntax private
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx data)
     (syntax-parse stx
       #:literals (implements method override property)
       [(_ (~and tag implements) form ...)
        (wrap-class-clause #`(private-implements . #,(parse-multiple-names #'(tag form ...))))]
       [(_ method (~var m (:method #'private))) #'m.form]
       [(_ override (~var m (:method #'private-override))) #'m.form]
       [(_ override method (~var m (:method #'private-override))) #'m.form]
       [(_ property (~var m (:method #'private-property))) #'m.form]
       [(_ override property (~var m (:method #'private-override-property))) #'m.form]
       [(_ (~and (~seq field _ ...) (~var f (:field 'private)))) #'f.form]
       [(_ (~var m (:method #'private))) #'m.form]))
   ;; interface clause
   (lambda (stx data)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'private))) #'m.form]
       [(_ (~var m (:method #'private))) #'m.form]))))
(define-syntax private-implements 'placeholder)
(define-syntax private-override 'placeholder)
(define-syntax private-property 'placeholder)
(define-syntax private-override-property 'placeholder)

(define-syntax abstract
  (make-class+interface-clause-transformer
   (lambda (stx data)
     (syntax-parse stx
       #:literals (method override property)
       [(_ method decl::method-decl) (wrap-class-clause #'(abstract decl.id decl.rhs decl.maybe-ret))]
       [(_ property decl::method-decl) (wrap-class-clause #'(abstract-property decl.id decl.rhs decl.maybe-ret))]
       [(_ override decl::method-decl) (wrap-class-clause #'(abstract-override decl.id decl.rhs decl.maybe-ret))]
       [(_ override method decl::method-decl) (wrap-class-clause #'(abstract-override decl.id decl.rhs decl.maybe-ret))]
       [(_ override property decl::method-decl) (wrap-class-clause #'(abstract-override-property decl.id decl.rhs decl.maybe-ret))]
       [(_ decl::method-decl) (wrap-class-clause #'(abstract decl.id decl.rhs decl.maybe-ret))]))))
(define-syntax abstract-property 'placeholder)
(define-syntax abstract-override 'placeholder)
(define-syntax abstract-override-property 'placeholder)

(define-for-syntax (same-return-signature? a b)
  (cond
    [(identifier? a)
     (and (identifier? b)
          (free-identifier=? a b))]
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
