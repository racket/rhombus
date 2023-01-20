#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/symbol
                     enforest
                     enforest/property
                     enforest/operator
                     enforest/transformer
                     enforest/transformer-result
                     enforest/proc-name
                     "introducer.rkt"
                     "name-path-op.rkt"
                     "pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-quasiquote)
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (for-syntax racket/base)
                     "implicit.rkt" ;; needed for `$%body`
                     "name-root.rkt"
                     "space.rkt"
                     "realm.rkt"
                     "parse.rkt")
         "enforest.rkt"
         "space-provide.rkt"
         "meta.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "definition.rkt"
         "space.rkt"
         "parens.rkt"
         "macro-macro.rkt")

(define+provide-space space rhombus/space
  #:fields
  (enforest
   transform))

(begin-for-syntax
  (define (id->string s) (symbol->immutable-string (syntax-e s)))

  (define-splicing-syntax-class :space_name
    #:attributes (name)
    #:datum-literals (/ op)
    (pattern (~seq root:identifier (~seq (op /) part:identifier) ...+)
             #:attr name (datum->syntax
                          #f
                          (string->symbol
                           (apply string-append
                                  (id->string #'root)
                                  (map id->string
                                       (syntax->list #'((~@ / part) ...)))))))
    (pattern (~seq name:identifier))))

(define-syntax enforest
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_  name:identifier
             (_::block
              (group #:space_path space-path::space_name)
              (~alt
               (~optional (group #:macro define-macro:identifier)
                          #:defaults ([define-macro #'SKIP])))
              ...
              (group #:meta_namespace meta-name:identifier
                     (_::block
                      (~alt
                       (~optional (group #:syntax_class class-name:identifier)
                                  #:defaults ([class-name #'Class]))
                       (~optional (group #:syntax_class_prefix_more prefix-more-class-name:identifier)
                                  #:defaults ([prefix-more-class-name #'PrefixMore]))
                       (~optional (group #:syntax_class_infix_more infix-more-class-name:identifier)
                                  #:defaults ([infix-more-class-name #'InfixMore]))
                       (~optional (group #:desc desc:string)
                                  #:defaults ([desc #'"form"]))
                       (~optional (group #:operator_desc desc-operator:string)
                                  #:defaults ([desc-operator #'"operator"]))
                       (~optional (group #:macro_result (check-at::block check-form ...))
                                  #:defaults ([check-at #'block] ; implicitly references `#%body`
                                              [(check-form 1) (list #'(group (parsed check-syntax)))]))
                       (~optional (group #:identifier_transformer (id-at::block make-identifier-form ...))
                                  #:defaults ([id-at #'block]
                                              [(make-identifier-form 1) (list #'(group (parsed values)))])))
                      ...))))
         #`((define-space-syntax name
              (space-syntax space-path.name))
            (define-name-root name
              #:fields
              #,(filter-missing
                 #`([define-macro _define-macro])))
            (begin-for-syntax
              (define-name-root meta-name
                #:fields
                #,(filter-missing
                   #`([class-name _class-name]
                      [prefix-more-class-name _prefix-more-class-name]
                      [infix-more-class-name _infix-more-class-name])))
              (define in-new-space (make-interned-syntax-introducer/add 'space-path.name))
              (property new-prefix-operator prefix-operator)
              (property new-infix-operator infix-operator)
              (struct new-prefix+infix-operator (prefix infix)
                #:property prop:new-prefix-operator (lambda (self) (new-prefix+infix-operator-prefix self))
                #:property prop:new-infix-operator (lambda (self) (new-prefix+infix-operator-infix self)))
              (define-rhombus-enforest
                #:syntax-class :base
                #:prefix-more-syntax-class :prefix-more
                #:infix-more-syntax-class :infix-more
                #:desc (quote desc)
                #:operator-desc (quote desc-operator)
                #:in-space in-new-space
                #:prefix-operator-ref new-prefix-operator-ref
                #:infix-operator-ref new-infix-operator-ref
                #:check-result (rhombus-body-at check-at check-form ...)
                #:make-identifier-form (rhombus-body-at id-at make-identifier-form ...))
              (define-syntax _class-name (make-syntax-class #':base
                                                            #:kind 'group
                                                            #:fields #'((parsed parsed parsed 0 unpack-term*))))
              (define-syntax _prefix-more-class-name (make-syntax-class #':prefix-more
                                                                        #:kind 'group
                                                                        #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                    (tail #f tail tail unpack-tail-list*))
                                                                        #:arity 2))
              (define-syntax _infix-more-class-name (make-syntax-class #':infix-more
                                                                       #:kind 'group
                                                                       #:fields #'((parsed parsed #f 0 unpack-term*)
                                                                                   (tail #f tail tail unpack-tail-list*))
                                                                       #:arity 2))
              (define make-prefix-operator (make-make-prefix-operator new-prefix-operator))
              (define make-infix-operator (make-make-infix-operator new-infix-operator)))
            (maybe-skip
             define-macro
             (define-operator-definition-transformer _define-macro
               'macro
               space-path.name
               #'make-prefix-operator
               #'make-infix-operator
               #'new-prefix+infix-operator)))]))))

(define-syntax SKIP 'placeholder)
(define-syntax (maybe-skip stx)
  (syntax-parse stx
    [(_ (~literal SKIP) . _) #'(begin)]
    [(_ _ def) #'def]))

(define-for-syntax (filter-missing flds)
  (for/list ([fld (in-list (syntax->list flds))]
             #:when (syntax-parse fld
                      [[#f . _] #f]
                      [_ #t]))
    fld))

(define-syntax transform
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ space::space_name
            (_::block
             (group #:space_path space-path::space_name)
             (~alt
              (~optional (group #:macro define-macro:identifier)
                         #:defaults ([define-macro #'SKIP])))
             ...)
            (group #:meta_namespace meta-name:identifier
                   (_::block
                    (~alt
                     (~optional (group #:syntax_class class-name)
                                #:defaults ([class-name #'Class]))
                     (~optional (group #:macro_result (check-at::block check-form ...))
                                #:defaults ([check-at #'block]
                                            [(check-form 1) (list #'(group (parsed check-syntax)))]))
                     (~optional (group #:desc desc:string)
                                #:defaults ([desc #'"form"])))
                    ...)))
         #'((define-name-root name
              #:root (space-syntax space-path.name)
              #:fields
              #,(filter-missing
                 #`([define-macro _define-macro])))
            (begin-for-syntax
              (define-name-root meta-name
                #:root (space-syntax space-path.name)
                #:fields
                #,(filter-missing
                   #`([class-name _class-name])))
              (define in-new-space (make-interned-syntax-introducer/add 'space.name))
              (property new-transformer transformer)
              (define-rhombus-transform
                #:syntax-class :base
                #:desc desc
                #:in-space in-new-space
                #:transformer-ref new-transformer-ref
                #:check-result (rhombus-body-at check-at check-form ...))
              (define-syntax _class-name (rhombus-syntax-class 'group #':base #'((parsed parsed 0 unpack-term*)) #f #f))
              (define make-transformer (make-make-transformer new-transformer)))
            (maybe-skip
             define-macro
             (define-identifier-syntax-definition-transformer _define-macro
               space.name
               #'make-transformer)))]))))

(define-for-syntax ((make-make-prefix-operator new-prefix-operator) name prec protocol proc)
  (new-prefix-operator
   name
   prec
   protocol
   (cond
     [(eq? protocol 'automatic) proc]
     [else
      (procedure-rename
       (lambda (tail)
         (finish (lambda () (syntax-parse tail
                              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)]))
                 proc))
       (object-name proc))])))

(define-for-syntax ((make-make-infix-operator new-infix-operator) name prec protocol proc assc)
  (new-infix-operator
   name
   prec
   protocol
   (cond
     [(eq? protocol 'automatic) proc]
     [else
      (procedure-rename
       (lambda (form1 tail)
         (finish
          (lambda () (syntax-parse tail
                       [(head . tail) (proc form1 (pack-tail #'tail #:after #'head) #'head)]))
          proc))
       (object-name proc))])
   assc))

(define-for-syntax ((make-make-transformer new-transformer) proc)
  (new-transformer
   (lambda (stx)
     (define r (syntax-parse stx
                 [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (check-syntax r proc)
     r)))

(define-for-syntax (finish thunk proc)
  (define-values (form new-tail)
    (call-with-values
     thunk
     (case-lambda
       [(form new-tail) (values form new-tail)]
       [(form) (values form #'(group))])))
  (values form
          (unpack-tail new-tail proc #f)))

(define-for-syntax (check-syntax form proc)
  (unless (syntax? form)
    (raise-result-error* (proc-name proc) rhombus-realm "Syntax" form))
  form)
