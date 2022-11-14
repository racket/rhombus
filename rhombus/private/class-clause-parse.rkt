#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "class-parse.rkt"
                     (only-in "rule.rkt" rule))
         "class-clause.rkt"
         "interface-clause.rkt"
         (only-in "binding.rkt" raise-binding-failure)
         (submod "annotation.rkt" for-class)
         "parens.rkt"
         "name-root-ref.rkt"
         "parse.rkt"
         (only-in "function.rkt" fun))

(provide (for-syntax extract-internal-ids
                     parse-options
                     wrap-class-clause)
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
         override
         private
         abstract)

(module+ for-interface
  (provide final-override))

(begin-for-syntax
  (struct class+interface-clause-transformer (cls int)
    #:property prop:class-clause-transformer (lambda (self) (class+interface-clause-transformer-cls self))
    #:property prop:interface-clause-transformer (lambda (self) (class+interface-clause-transformer-int self)))
  (define (make-class+interface-clause-transformer proc [int-proc proc])
    (class+interface-clause-transformer
     (class-clause-transformer proc)
     (interface-clause-transformer int-proc))))

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
  
(define-for-syntax (parse-options orig-stx forms)
  (syntax-parse forms
    #:context orig-stx
    [((_ clause-parsed) ...)
     (define clauses (syntax->list #'(clause-parsed ...)))
     (define (extract-rhs b)
       (syntax-parse b
         [(_::block g) #'g]
         [else
          (raise-syntax-error #f
                              "expected a single entry point in block body"
                              b)]))
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
                                  method private override abstract internal
                                  final-override)
              [(extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "multiple extension clauses" orig-stx clause))
               (hash-set options 'extends #'id)]
              [(implements id ...)
               (add-implements options 'public-implements #'(id ...))]
              [(private-implements id ...)
               (add-implements options 'private-implements #'(id ...))]
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "multiple internal-name clauses" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(constructor _ rhs)
               (when (hash-has-key? options 'constructor-rhs)
                 (raise-syntax-error #f "multiple constructor clauses" orig-stx clause))
               (hash-set options 'constructor-rhs #'rhs)]
              [(binding block)
               (when (hash-has-key? options 'binding-rhs)
                 (raise-syntax-error #f "multiple binding clauses" orig-stx clause))
               (hash-set options 'binding-rhs (extract-rhs #'block))]
              [(annotation block)
               (when (hash-has-key? options 'annotation-rhs)
                 (raise-syntax-error #f "multiple annotation clauses" orig-stx clause))
               (hash-set options 'annotation-rhs (extract-rhs #'block))]
              [(nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "multiple finality clauses" orig-stx clause))
               (hash-set options 'final? #f)]
              [(authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "multiple authenticity clause" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(field id rhs-id static-infos predicate annotation-str mode)
               (hash-set options 'fields (cons (added-field #'id
                                                            #'rhs-id
                                                            #'static-infos
                                                            #'predicate
                                                            #'annotation-str
                                                            (syntax-e #'mode))
                                               (hash-ref options 'fields null)))]
              [((~and tag (~or method override private final final-override private-override)) id rhs)
               (hash-set options 'methods (cons (added-method #'id
                                                              (car (generate-temporaries #'(id)))
                                                              #'rhs
                                                              (syntax-e #'tag))
                                                (hash-ref options 'methods null)))]
              [(abstract id)
               (hash-set options 'methods (cons (added-method #'id
                                                              '#:abstract
                                                              #f
                                                              'abstract)
                                                (hash-ref options 'methods null)))]
              [_
               (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (wrap-class-clause parsed)
  #`[(quote-syntax (rhombus-class #,parsed) #:local)]) ; `quote-syntax` + `rhombus-class` wrapper => clause

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
   (lambda (stx)
     (syntax-parse stx
       [(_ (~seq form ...))
        #:with (~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref)) #'(form ...)
        #:with () #'id.tail
        (wrap-class-clause #'(extends id.name))]))
   ;; interface clause
   (lambda (stx)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(extends . #,names)))))

(define-syntax implements
  (class-clause-transformer
   (lambda (stx)
     (define names (parse-multiple-names stx))
     (wrap-class-clause #`(implements . #,names)))))

(define-syntax internal
  (make-class+interface-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(internal name))]))))

(define-syntax binding
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (~and (_::quotes . _)
                         pattern)
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(binding (block (group rule pattern template-block))))]
       [(form-name (~and (_::block . _)
                         binding-block))
        (wrap-class-clause #`(binding binding-block))]))))

(define-syntax annotation
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-name (~and (_::quotes . _)
                         pattern)
                   (~and (_::block . _)
                         template-block))
        (wrap-class-clause #`(annotation (block (group rule pattern template-block))))]
       [(form-name (~and (_::block . _)
                         annotation-block))
        (wrap-class-clause #`(annotation annotation-block))]))))

(define-syntax nonfinal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(nonfinal))]))))

(define-syntax authentic
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_) (wrap-class-clause #`(authentic))]))))

(begin-for-syntax
  (define-splicing-syntax-class (:field mode)
    #:description "field identifier with optional annotation"
    #:attributes (form)
    (pattern (~seq form-id bind ...
                   (~and blk (_::block . _)))
             #:with (id:identifier (~optional c::inline-annotation)) #'(bind ...)
             #:attr predicate (if (attribute c)
                                  #'c.predicate
                                  #'#f)
             #:attr annotation-str (if (attribute c)
                                       #'c.annotation-str
                                       #'#f)
             #:attr static-infos (if (attribute c)
                                     #'c.static-infos
                                     #'())
             #:attr form
             #`[(define tmp-id (let ([f-info.name-id (rhombus-body-at . blk)])
                                 {~? (if (c.predicate f-info.name-id)
                                         f-info.name-id
                                         (raise-binding-failure 'form-id "value" f-info.name-id 'c.annotation-str))
                                     f-info.name-id}))
                #,@(wrap-class-clause #`(field id
                                               tmp-id
                                               static-infos
                                               predicate
                                               annotation-str
                                               #,mode))])))

(define-syntax field
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [((~var f (:field 'public)))
        #'f.form]))))

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))

(begin-for-syntax
  (define-splicing-syntax-class (:method mode)
    #:description "method declaration"
    #:attributes (form)
    (pattern (~seq id:identifier (tag::parens arg ...) ret ...
                   (~and rhs (_::block body ...)))
             #:attr form (wrap-class-clause #`(#,mode id (block (group fun (tag arg ...) ret ... rhs)))))
    (pattern (~seq id:identifier (~and rhs (_::block . _)))
             #:attr form (wrap-class-clause #`(#,mode id rhs)))))

(define-syntax constructor
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [((~var m (:method #'constructor))) #'m.form]))))

(define-syntax final
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx)
     (syntax-parse stx
       #:literals (override method)
       [(_ override method (~var m (:method #'final-override))) #'m.form]
       [(_ method (~var m (:method #'final))) #'m.form]
       [(_ override (~var m (:method #'final-override))) #'m.form]
       [(_ (~var m (:method #'final))) #'m.form]))
   ;; interface clause
   (lambda (stx)
     (syntax-parse stx
       #:literals (override method)
       [(_ override method (~var m (:method #'final-override))) #'m.form]
       [(_ method (~var m (:method #'final))) #'m.form]
       [(_ override (~var m (:method #'final-override))) #'m.form]
       [(_ (~var m (:method #'final))) #'m.form]))))
(define-syntax final-override 'placeholder)

(define-syntax method
  (make-class+interface-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~var m (:method #'method))) #'m.form]))))

(define-syntax override
  (make-class+interface-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'override))) #'m.form]
       [(_ (~var m (:method #'override))) #'m.form]))))

(define-syntax private
  (make-class+interface-clause-transformer
   ;; class clause
   (lambda (stx)
     (syntax-parse stx
       #:literals (implements method override)
       [(_ (~and tag implements) form ...)
        (wrap-class-clause #`(private-implements . #,(parse-multiple-names #'(tag form ...))))]
       [(_ method (~var m (:method #'private))) #'m.form]
       [(_ override (~var m (:method #'private-override))) #'m.form]
       [(_ override method (~var m (:method #'private-override))) #'m.form]
       [(_ (~and (~seq field _ ...) (~var f (:field 'private)))) #'f.form]
       [(_ (~var m (:method #'private))) #'m.form]))
   ;; interface clause
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'private))) #'m.form]
       [(_ (~var m (:method #'private))) #'m.form]))))
(define-syntax private-implements 'placeholder)

(define-syntax abstract
  (make-class+interface-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method name:identifier) (wrap-class-clause #'(abstract name))]
       [(_ name:identifier) (wrap-class-clause #'(abstract name))]))))
