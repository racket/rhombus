#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/proc-name
                     "pack.rkt"
                     "static-info-pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "tail-returner.rkt"
                     "realm.rkt"
                     "macro-result.rkt")
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "definition.rkt"
         (only-in "binding.rkt" :binding-form)
         (submod "annotation.rkt" for-class)
         "macro-macro.rkt"
         "wrap-expression.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         "parse.rkt"
         "annot-delayed.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                annot_meta)))

(module+ for-class
  (provide (for-syntax make-annotation-prefix-operator)))

(define+provide-space annot rhombus/annot
  #:fields
  (macro
   delayed_declare
   delayed_complete))

(begin-for-syntax
  (define-name-root annot_meta
    #:fields
    (space
     is_predicate
     pack_predicate
     unpack_predicate
     is_converter
     pack_converter
     unpack_converter
     Parsed
     AfterPrefixParsed
     AfterInfixParsed)))

(define-for-syntax space
  (space-syntax rhombus/annot))

(define-operator-definition-transformer macro
  'macro
  rhombus/annot
  #'make-annotation-prefix-operator
  #'make-annotation-infix-operator
  #'annotation-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :annotation #:rhombus/annot
    AfterPrefixParsed :prefix-op+annotation+tail
    AfterInfixParsed :infix-op+annotation+tail))

(begin-for-syntax
  (struct annotation-prefix+infix-operator (prefix infix)
    #:property prop:annotation-prefix-operator (lambda (self) (annotation-prefix+infix-operator-prefix self))
    #:property prop:annotation-infix-operator (lambda (self) (annotation-prefix+infix-operator-infix self))))

(define-for-syntax (parse-annotation-macro-result form proc)
  (unless (syntax? form)
    (raise-bad-macro-result (proc-name proc) "annotation" form))
  (syntax-parse (unpack-group form proc #f)
    [c::annotation #'c.parsed]))

(define-for-syntax (make-annotation-infix-operator name prec protocol proc assc)
  (annotation-infix-operator
   name
   prec
   protocol
   (lambda (form1 tail)
     (define-values (form new-tail)
       (tail-returner
        proc
        (syntax-parse tail
          [(head . tail) (proc #`(parsed #:rhombus/annot #,form1) (pack-tail #'tail) #'head)])))
     (check-transformer-result (parse-annotation-macro-result form proc)
                               (unpack-tail new-tail proc #f)
                               proc))
   assc))

(define-for-syntax (make-annotation-prefix-operator name prec protocol proc)
  (annotation-prefix-operator
   name
   prec
   protocol
   (lambda (tail)
     (define-values (form new-tail)
       (tail-returner
        proc
        (syntax-parse tail
          [(head . tail) (proc (pack-tail #'tail) #'head)])))
     (check-transformer-result (parse-annotation-macro-result form proc)
                               (unpack-tail new-tail proc #f)
                               proc))))
  
(define-for-syntax (annotation-kind stx who)
  (syntax-parse (unpack-term stx who #f)
    #:datum-literals (parsed)
    [(parsed #:rhombus/annot a::annotation-predicate-form) 'predicate]
    [(parsed #:rhombus/annot a::annotation-binding-form) 'converter]
    [_ (raise-arguments-error* who
                               rhombus-realm
                               "syntax object is not a parsed annotation"
                               "syntax object" stx)]))

(define-for-syntax (is_predicate stx)
  (eq? (annotation-kind stx 'annot_meta.is_predicate) 'predicate))

(define-for-syntax (pack_predicate predicate [static-infos #'(parens)])
  (unless (syntax? predicate) (raise-argument-error* 'annot.pack_predicate rhombus-realm "Syntax" predicate))
  #`(parsed #:rhombus/annot #,(annotation-predicate-form (wrap-expression predicate)
                                                         (pack-static-infos (unpack-term static-infos 'annot.pack_predicate #f)
                                                                            'annot.pack_predicate))))

(define-for-syntax (unpack_predicate stx)
  (syntax-parse (unpack-term stx 'annot_meta.unpack_predicate #f)
    #:datum-literals (parsed)
    [(parsed #:rhombus/annot a::annotation-predicate-form)
     (values #'(parsed #:rhombus/expr a.predicate)
             (unpack-static-infos #'a.static-infos))]
    [_
     (raise-arguments-error* 'annot_meta.unpack_predicate
                             rhombus-realm
                             "syntax object is not a parsed annotation that is a predicate"
                             "syntax object" stx)]))

(define-for-syntax (is_converter stx)
  (eq? (annotation-kind stx 'annot_meta.is_converter) 'converter))

(define-for-syntax (pack_converter binding body [static-infos #'(parens)])
  (syntax-parse (if (syntax? binding) binding #'no)
    [(parsed #:rhombus/bind _::binding-form)
     (unless (syntax? body) (raise-argument-error* 'annot.pack_converter rhombus-realm "Syntax" body))
     #`(parsed #:rhombus/annot
               #,(annotation-binding-form binding
                                          (wrap-expression body)
                                          (pack-static-infos (unpack-term static-infos 'annot.pack_converter #f)
                                                             'annot.pack_converter)))]
    [_ (raise-arguments-error* 'annot_meta.pack_converter
                               rhombus-realm
                               "not a parsed-binding syntax object"
                               "value"  binding)]))
     

(define-for-syntax (unpack_converter stx)
  (syntax-parse (unpack-term stx 'annot_meta.unpack_predicate #f)
    #:datum-literals (parsed)
    [(parsed #:rhombus/annot a::annotation-binding-form)
     (values #'(parsed #:rhombus/bind a.binding)
             #'(parsed #:rhombus/expr a.body)
             (unpack-static-infos #'a.static-infos))]
    [_
     (raise-arguments-error* 'annot_meta.unpack_converter
                             rhombus-realm
                             "syntax object is not a parsed annotation that is a converter"
                             "syntax object" stx)]))
