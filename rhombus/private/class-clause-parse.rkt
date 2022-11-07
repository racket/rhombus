#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/hier-name-parse
                     "srcloc.rkt"
                     "name-path-op.rkt"
                     "class-parse.rkt")
         "class-clause.rkt"
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
         unimplemented)

(define-for-syntax (extract-internal-ids options
                                         scope-stx base-stx
                                         stxes)
  (define internal-id (hash-ref options 'internal #f))
  (define expose (if internal-id
                     (let ([intro (make-syntax-delta-introducer scope-stx base-stx)])
                       (lambda (stx)
                         (intro stx 'remove)))
                     (lambda (stx) stx)))
  (define (maybe-use-internal-id id clause-name)
    (cond
      [(syntax-e id) id]
      [internal-id internal-id]
      [else (raise-syntax-error #f
                                (format "no `internal` clause, and no maker name in `~a` clause"
                                        clause-name)
                                stxes)]))
  (values internal-id
          (expose internal-id)
          (let ([id (hash-ref options 'constructor-id #f)])
            (and id
                 (maybe-use-internal-id id 'constructor)))
          (let ([b (hash-ref options 'binding #f)])
            (and b
                 (maybe-use-internal-id (car b) 'binding)))
          (let ([b (hash-ref options 'annotation #f)])
            (and b
                 (maybe-use-internal-id (car b) 'annotation)))))
  
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
     (let loop ([clauses clauses] [options #hasheq()])
       (cond
         [(null? clauses) options]
         [else
          (define clause (car clauses))
          (define new-options
            (syntax-parse clause
              #:literals (extends constructor final nonfinal authentic binding annotation
                                  method private override unimplemented internal
                                  final-override)
              [(extends id)
               (when (hash-has-key? options 'extends)
                 (raise-syntax-error #f "redundant superclass clause" orig-stx clause))
               (hash-set options 'extends #'id)]
              [(internal id)
               (when (hash-has-key? options 'internal)
                 (raise-syntax-error #f "redundant internal-name clause" orig-stx clause))
               (hash-set options 'internal #'id)]
              [(constructor id block)
               (when (hash-has-key? options 'constructor-id)
                 (raise-syntax-error #f "redundant constructor clause" orig-stx clause))
               (hash-set (hash-set options 'constructor-id #'id)
                         'constructor-rhs
                         (extract-rhs #'block))]
              [(binding core-name block)
               (when (hash-has-key? options 'binding)
                 (raise-syntax-error #f "redundant binding clause" orig-stx clause))
               (hash-set options 'binding (list #'core-name (extract-rhs #'block)))]
              [(annotation core-name block)
               (when (hash-has-key? options 'annotation)
                 (raise-syntax-error #f "redundant annotation clause" orig-stx clause))
               (hash-set options 'annotation (list #'core-name (extract-rhs #'block)))]
              [(final)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #t)]
              [(nonfinal)
               (when (hash-has-key? options 'final?)
                 (raise-syntax-error #f "redundant finality clause" orig-stx clause))
               (hash-set options 'final? #f)]
              [(authentic)
               (when (hash-has-key? options 'authentic?)
                 (raise-syntax-error #f "redundant authenticity clause" orig-stx clause))
               (hash-set options 'authentic? #t)]
              [(field id rhs-id static-infos predicate annotation-str mode)
               (hash-set options 'fields (cons (added-field #'id
                                                            #'rhs-id
                                                            #'static-infos
                                                            #'predicate
                                                            #'annotation-str
                                                            (syntax-e #'mode))
                                               (hash-ref options 'fields null)))]
              [((~and tag (~or method override private final final-override)) id rhs)
               (hash-set options 'methods (cons (added-method #'id
                                                              (car (generate-temporaries #'(id)))
                                                              #'rhs
                                                              (syntax-e #'tag))
                                                (hash-ref options 'methods null)))]
              [(unimplemented id)
               (hash-set options 'methods (cons (added-method #'id
                                                              (car (generate-temporaries #'(id)))
                                                              #f
                                                              'unimplemented)
                                                (hash-ref options 'methods null)))]
              [_
               (raise-syntax-error #f "unrecognized clause" orig-stx clause)]))
          (loop (cdr clauses) new-options)]))]))

(define-for-syntax (wrap-class-clause parsed)
  #`[(quote-syntax (rhombus-class #,parsed) #:local)]) ; `quote-syntax` + `rhombus-class` wrapper => clause

(define-syntax extends
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~seq form ...))
        #:with (~var id (:hier-name-seq in-class-desc-space name-path-op name-root-ref)) #'(form ...)
        #:with () #'id.tail
        (wrap-class-clause #'(extends id.name))]))))

(define-syntax internal
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ name:identifier)
        (wrap-class-clause #'(internal name))]))))

(define-syntax constructor
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group make:identifier))
           (~and (_::block . _)
                 constructor-block))
        (wrap-class-clause #`(constructor make constructor-block))]
       [(_ (~and (_::block . _)
                 constructor-block))
        (wrap-class-clause #`(constructor #f constructor-block))]))))

(define-syntax binding
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 binding-block))
        (wrap-class-clause #`(binding core binding-block))]
       [(_ (~and (_::block . _)
                 binding-block))
        (wrap-class-clause #`(binding #f binding-block))]))))

(define-syntax annotation
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group core:identifier))
           (~and (_::block . _)
                 annotation-block))
        (wrap-class-clause #`(annotation core annotation-block))]
       [(_ (~and (_::block . _)
                 annotation-block))
        (wrap-class-clause #`(annotation #f annotation-block))]))))

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

(define-syntax final
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (override method)
       [(_) (wrap-class-clause #`(final))]
       [(_ override method (~var m (:method #'final-override))) #'m.form]
       [(_ method (~var m (:method #'final))) #'m.form]
       [(_ override (~var m (:method #'final-override))) #'m.form]
       [(_ (~var m (:method #'final))) #'m.form]))))
(define-syntax final-override 'placeholder)

(define-syntax method
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (~var m (:method #'method))) #'m.form]))))

(define-syntax override
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'override))) #'m.form]
       [(_ (~var m (:method #'override))) #'m.form]))))

(define-syntax private
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method (~var m (:method #'private))) #'m.form]
       [(_ (~and (~seq field _ ...) (~var f (:field 'private)))) #'f.form]
       [(_ (~var m (:method #'private))) #'m.form]))))

(define-syntax unimplemented
  (class-clause-transformer
   (lambda (stx)
     (syntax-parse stx
       #:literals (method)
       [(_ method name:identifier) (wrap-class-clause #'(unimplemented name))]
       [(_ name:identifier) (wrap-class-clause #'(unimplemented name))]))))

