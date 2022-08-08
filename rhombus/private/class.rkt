#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/syntax-local
                     "srcloc.rkt")
         "definition.rkt"
         "expression.rkt"
         "binding.rkt"
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "composite.rkt"
         "assign.rkt"
         "static-info.rkt"
         "realm.rkt")

(provide (rename-out [rhombus-class class]))

(begin-for-syntax
  (struct class-desc (constructor-id fields))
  (define (class-desc-ref v) (and (class-desc? v) v))

  (define in-class-desc-space (make-interned-syntax-introducer 'rhombus/class))

  (define-syntax-class :field
    #:datum-literals (group op)
    #:literals (mutable)
    (pattern (group (~optional (~and mutable (~var mutable))
                               #:defaults ([mutable #'#f]))
                    name:identifier
                    (~optional c::inline-annotation))
             #:attr predicate (if (attribute c)
                                  #'c.predicate
                                  #'#f)
             #:attr annotation-str (if (attribute c)
                                       #'c.annotation-str
                                       #'#f)
             #:attr static-infos (if (attribute c)
                                     #'c.static-infos
                                     #'()))))

(define-syntax rhombus-class
  (definition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ name:identifier ((~datum parens) field::field ...))
        (define fields (syntax->list #'(field.name ...)))
        (define-values (immutable-fields mutable-fields)
          (for/fold ([imm '()] [m '()] #:result (values (reverse imm) (reverse m)))
                    ([field (in-list fields)]
                     [mutable (syntax->list #'(field.mutable ...))])
            (if (syntax-e mutable)
                (values imm (cons field m))
                (values (cons field imm) m))))
        (with-syntax ([name? (datum->syntax #'name (string->symbol (format "~a?" (syntax-e #'name))) #'name)]
                      [(class:name) (generate-temporaries #'(name))]
                      [name-instance (datum->syntax #'name (string->symbol (format "~a.instance" (syntax-e #'name))) #'name)]
                      [(name-field ...) (for/list ([field (in-list fields)])
                                          (datum->syntax field
                                                         (string->symbol (format "~a.~a"
                                                                                 (syntax-e #'name)
                                                                                 (syntax-e field)))
                                                         field))]
                      [(set-name-field! ...) (for/list ([field (in-list mutable-fields)])
                                               (datum->syntax field
                                                              (string->symbol (format "set-~a.~a!"
                                                                                      (syntax-e #'name)
                                                                                      (syntax-e field)))
                                                              field))]
                      [cnt (length fields)]
                      [(field-index ...) (for/list ([field (in-list fields)]
                                                    [i (in-naturals)])
                                           i)]
                      [(immutable-field-index ...) (for/list ([field (in-list immutable-fields)]
                                                              [i (in-naturals)])
                                                     i)]
                      [(mutable-field ...) mutable-fields]
                      [(mutable-field-index ...) (for/list ([mutable (syntax->list #'(field.mutable ...))]
                                                            [i (in-naturals)]
                                                            #:when (syntax-e mutable))
                                                   i)])
          (list
           #`(define-values (class:name name name? name-field ... set-name-field! ...)
               (let-values ([(class:name name name? name-ref name-set!)
                             (make-struct-type 'name #f cnt 0 #f
                                               (list (cons prop:field-name->accessor '(field.name ...)))
                                               #f #f
                                               '(immutable-field-index ...)
                                               #,(build-guard-expr fields
                                                                   (syntax->list #'(field.predicate ...))
                                                                   (map syntax-e
                                                                        (syntax->list #'(field.annotation-str ...)))))])
                 (values class:name name name?
                         (make-struct-field-accessor name-ref field-index 'name-field 'name 'rhombus)
                         ...
                         (make-struct-field-mutator name-set! mutable-field-index 'set-name-field! 'name 'rhombus)
                         ...)))
           #`(define-binding-syntax name
               (binding-transformer
                #'name
                (make-composite-binding-transformer #,(symbol->string (syntax-e #'name))
                                                    (quote-syntax name?)
                                                    #:static-infos (quote-syntax ((#%dot-provider name-instance)))
                                                    (list (quote-syntax name-field) ...)
                                                    #:accessor->info? #t
                                                    (list (quote-syntax field.static-infos) ...))))
           #'(define-annotation-constructor name
               ([accessors (list (quote-syntax name-field) ...)])
               (quote-syntax name?)
               (quote-syntax ((#%dot-provider name-instance)))
               cnt
               (make-class-instance-predicate accessors)
               (make-class-instance-static-infos accessors))
           #'(define-class-desc-syntax name
               (class-desc (quote-syntax name)
                           (list (list 'field.name (quote-syntax name-field) (quote-syntax field.static-infos))
                                 ...)))
           #'(define-dot-provider-syntax name
               (dot-provider (make-handle-class-type-dot (quote-syntax name))))
           #'(define-dot-provider-syntax name-instance
               (dot-provider (make-handle-class-instance-dot (quote-syntax name))))
           #'(define-static-info-syntax name (#%call-result ((#%dot-provider name-instance))))
           #'(begin
               (define-static-info-syntax/maybe* name-field (#%call-result field.static-infos))
               ...)))]))))

(define-for-syntax (build-guard-expr fields predicates annotation-strs)
  (and (for/or ([predicate (in-list predicates)])
         (syntax-e predicate))
       #`(lambda (#,@fields who)
           (values #,@(for/list ([field (in-list fields)]
                                 [predicate (in-list predicates)]
                                 [annotation-str (in-list annotation-strs)])
                        (cond
                          [(not (syntax-e predicate)) field]
                          [else #`(if (#,predicate #,field)
                                      #,field
                                      (raise-argument-error* who
                                                             rhombus-realm
                                                             '#,annotation-str
                                                             #,field))]))))))

(define-for-syntax (make-class-instance-predicate accessors)
  (lambda (arg predicate-stxs)
    #`(and #,@(for/list ([acc (in-list accessors)]
                         [pred (in-list predicate-stxs)])
                #`(#,pred (#,acc #,arg))))))

(define-for-syntax (make-class-instance-static-infos accessors)
  (lambda (statis-infoss)
    (for/list ([acc (in-list accessors)]
               [static-infos (in-list statis-infoss)])
      #`(#,acc #,static-infos))))

;; dot provider for a class name used before a `.`
(define-for-syntax ((make-handle-class-type-dot name) form1 dot field-id)
  (define desc (syntax-local-value* (in-class-desc-space name) class-desc-ref))
  (unless desc (error "cannot find annotation binding for dot provider"))
  (define accessor-id
    (for/or ([field+acc (in-list (class-desc-fields desc))])
      (and (eq? (car field+acc) (syntax-e field-id))
           (cadr field+acc))))
  (unless accessor-id
    (raise-syntax-error #f
                        "cannot find field in class"
                        field-id))
  (relocate (span-srcloc form1 field-id) accessor-id))

;; dot provider for a class instance used before a `.`
(define-for-syntax ((make-handle-class-instance-dot name) form1 dot field-id)
  (define desc (syntax-local-value* (in-class-desc-space name) class-desc-ref))
  (unless desc (error "cannot find annotation binding for instance dot provider"))
  (define accessor-id+static-infos
    (for/or ([field+acc (in-list (class-desc-fields desc))])
      (and (eq? (car field+acc) (syntax-e field-id))
           (cdr field+acc))))
  (cond
    [accessor-id+static-infos
     (define accessor-id (car accessor-id+static-infos))
     (define e (datum->syntax (quote-syntax here)
                              (list (relocate field-id accessor-id) form1)
                              (span-srcloc form1 field-id)
                              #'dot))

     (define static-infos (cadr accessor-id+static-infos))
     (define more-static-infos (syntax-local-static-info form1 accessor-id))
     (define all-static-infos (if more-static-infos
                                  (datum->syntax #f
                                                 (append (syntax->list more-static-infos)
                                                         static-infos))
                                  static-infos))

     (wrap-static-info* e all-static-infos)]
    [else #f]))

(define-syntax (define-class-desc-syntax stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(define-syntax #,(in-class-desc-space #'id)
         rhs)]))

(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(_ id (_)) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))
