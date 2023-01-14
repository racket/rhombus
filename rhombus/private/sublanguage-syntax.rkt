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
                     (submod "syntax-class.rkt" for-quasiquote)
                     (for-syntax racket/base)
                     "realm.rkt"
                     "parse.rkt")
         "name-root.rkt"
         "name-root-ref.rkt"
         "definition.rkt"
         "parens.rkt"
         "syntax.rkt")

(provide sublanguage)

(define-name-root sublanguage
  #:fields (enforest
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
        [(_ space::space_name (_::block
                               (~alt
                                (~optional (group #:syntax_class class-name:identifier)
                                           #:defaults ([class-name #'Class]))
                                (~optional (group #:syntax_class_prefix_more prefix-more-class-name:identifier)
                                           #:defaults ([prefix-more-class-name #'PrefixMore]))
                                (~optional (group #:syntax_class_infix_more infix-more-class-name:identifier)
                                           #:defaults ([infix-more-class-name #'InfixMore]))
                                (~optional (group #:macro define-macro:identifier)
                                           #:defaults ([define-macro #'SKIP]))
                                (~optional (group #:only_macro define-only-macro:identifier)
                                           #:defaults ([define-only-macro #'SKIP]))
                                (~optional (group #:rule define-rule:identifier)
                                           #:defaults ([define-rule #'SKIP]))
                                (~optional (group #:only_rule define-only-rule:identifier)
                                           #:defaults ([define-only-rule #'SKIP]))
                                (~optional (group #:desc desc:string)
                                           #:defaults ([desc #'"form"]))
                                (~optional (group #:operator_desc desc-operator:string)
                                           #:defaults ([desc-operator #'"operator"]))
                                (~optional (group #:macro_result (check-at::block check-form ...))
                                           #:defaults ([check-at #'block]
                                                       [(check-form 1) (list #'(group (parsed check-syntax)))]))
                                (~optional (group #:identifier_transformer (id-at::block make-identifier-form ...))
                                           #:defaults ([id-at #'block]
                                                       [(make-identifier-form 1) (list #'(group (parsed values)))])))
                               ...))
         #'((begin-for-syntax
              (define in-new-space (make-interned-syntax-introducer/add 'space.name))
              (property new-prefix-operator prefix-operator)
              (property new-infix-operator infix-operator)
              (struct new-prefix+infix-operator (prefix infix)
                #:property prop:new-prefix-operator (lambda (self) (new-prefix+infix-operator-prefix self))
                #:property prop:new-infix-operator (lambda (self) (new-prefix+infix-operator-infix self)))
              (define-enforest
                #:syntax-class :base
                #:prefix-more-syntax-class :prefix-more
                #:infix-more-syntax-class :infix-more
                #:desc (quote desc)
                #:operator-desc (quote desc-operator)
                #:in-space in-new-space
                #:name-path-op name-path-op
                #:name-root-ref name-root-ref
                #:name-root-ref-root name-root-ref-root
                #:prefix-operator-ref new-prefix-operator-ref
                #:infix-operator-ref new-infix-operator-ref
                #:check-result (rhombus-body-at check-at check-form ...)
                #:make-identifier-form (rhombus-body-at id-at make-identifier-form ...))
              (define-syntax-class :prefix-more*
                #:datum-literals (group)
                (pattern (group . r)
                         #:with a::prefix-more #'r
                         #:attr parsed #'a.parsed
                         #:attr tail (pack-tail #'a.tail)))
              (define-syntax-class :infix-more*
                #:datum-literals (group op)
                (pattern (group . r)
                         #:with a::infix-more #'r
                         #:attr parsed #'a.parsed
                         #:attr tail (pack-tail #'a.tail)))
              (define-syntax class-name (rhombus-syntax-class 'group #':base '((parsed . 0)) #f #f))
              (define-syntax prefix-more-class-name (rhombus-syntax-class 'group #':prefix-more* '((parsed . 0) (tail . 0)) #f #f))
              (define-syntax infix-more-class-name (rhombus-syntax-class 'group #':infix-more* '((parsed . 0) (tail . 0)) #f #f))
              (define make-prefix-operator (make-make-prefix-operator new-prefix-operator))
              (define make-infix-operator (make-make-infix-operator new-infix-operator)))
            (maybe-skip
             (define-operator-definition-transformer define-macro
               'macro
               #f
               #'make-prefix-operator
               #'make-infix-operator
               #'new-prefix+infix-operator))
            (maybe-skip
             (define-operator-definition-transformer define-only-macro
               'macro
               space.name
               #'make-prefix-operator
               #'make-infix-operator
               #'new-prefix+infix-operator))
            (maybe-skip
             (define-operator-definition-transformer define-rule
               'rule
               #f
               #'make-prefix-operator
               #'make-infix-operator
               #'new-prefix+infix-operator))
            (maybe-skip
             (define-operator-definition-transformer define-only-rule
               'rule
               space.name
               #'make-prefix-operator
               #'make-infix-operator
               #'new-prefix+infix-operator)))]))))

(define-syntax SKIP 'placeholder)
(define-syntax (maybe-skip stx)
  (syntax-parse stx
    [(_ (_ (~literal SKIP) . _)) #'(begin)]
    [(_ def) #'def]))

(define-syntax transform
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ space::space_name (_::block
                               (~alt
                                (~optional (group #:syntax_class class-name)
                                           #:defaults ([class-name #'Class]))
                                (~optional (group #:macro define-macro:identifier)
                                           #:defaults ([define-macro #'SKIP]))
                                (~optional (group #:only_macro define-only-macro:identifier)
                                           #:defaults ([define-only-macro #'SKIP]))
                                (~optional (group #:macro_result (check-at::block check-form ...))
                                           #:defaults ([check-at #'block]
                                                       [(check-form 1) (list #'(group (parsed check-syntax)))]))
                                (~optional (group #:desc desc:string)
                                           #:defaults ([desc #'"form"])))
                               ...))
         #'((begin-for-syntax
              (define in-new-space (make-interned-syntax-introducer/add 'space.name))
              (property new-transformer transformer)
              (define-transform
                #:syntax-class :base
                #:desc desc
                #:in-space in-new-space
                #:name-path-op name-path-op
                #:name-root-ref name-root-ref
                #:name-root-ref-root name-root-ref-root
                #:transformer-ref new-transformer-ref
                #:check-result (rhombus-body-at check-at check-form ...))
              (define-syntax class-name (rhombus-syntax-class 'group #':base '((parsed . 0)) #f #f))
              (define make-transformer (make-make-transformer new-transformer)))
            (maybe-skip
             (define-identifier-syntax-definition-transformer define-macro
               #f
               #'make-transformer))
            (maybe-skip
             (define-identifier-syntax-definition-transformer define-macro-only
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
