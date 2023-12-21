#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/proc-name
                     "name-root.rkt"
                     "pack.rkt"
                     "static-info-pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "tail-returner.rkt"
                     "macro-result.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "values-key.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         (only-in "binding.rkt" :binding-form)
         (submod "annotation.rkt" for-class)
         "macro-macro.rkt"
         "wrap-expression.rkt"
         "annot-delayed.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                annot_meta)))

(module+ for-class
  (provide (for-syntax make-annotation-prefix-operator)))

(define+provide-space annot rhombus/annot
  #:fields
  (macro
   ;; "annot-delayed.rkt"
   delayed_declare
   delayed_complete))

(begin-for-syntax
  (define-name-root annot_meta
    #:fields
    (space
     [is_predicate annot_meta.is_predicate]
     [pack_predicate annot_meta.pack_predicate]
     [unpack_predicate annot_meta.unpack_predicate]
     [is_converter annot_meta.is_converter]
     [pack_converter annot_meta.pack_converter]
     [unpack_converter annot_meta.unpack_converter]
     [parse_to_packed_statinfo annot_meta.parse_to_packed_statinfo]
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

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(define-for-syntax (annotation-kind stx who)
  (check-syntax who stx)
  (syntax-parse (unpack-term stx who #f)
    #:datum-literals (parsed)
    [(parsed #:rhombus/annot a::annotation-predicate-form) 'predicate]
    [(parsed #:rhombus/annot a::annotation-binding-form) 'converter]
    [_ (raise-arguments-error* who rhombus-realm
                               "not a parsed annotation"
                               "syntax object" stx)]))

(begin-for-syntax
  (define/arity (annot_meta.is_predicate stx)
    (eq? (annotation-kind stx who) 'predicate))

  (define/arity (annot_meta.pack_predicate predicate [static-infos #'(parens)])
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who predicate)
    (check-syntax who static-infos)
    #`(parsed #:rhombus/annot
              #,(annotation-predicate-form
                 (wrap-expression predicate)
                 (pack-static-infos who (unpack-term static-infos who #f)))))

  (define/arity (annot_meta.unpack_predicate stx)
    #:static-infos ((#%call-result ((#%values (#,syntax-static-infos
                                               #,syntax-static-infos)))))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/annot a::annotation-predicate-form)
       (values #'(parsed #:rhombus/expr a.predicate)
               (unpack-static-infos who #'a.static-infos))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed predicate annotation"
                                 "syntax object" stx)]))

  (define/arity (annot_meta.is_converter stx)
    (eq? (annotation-kind stx who) 'converter))

  (define/arity (annot_meta.pack_converter binding body [static-infos #'(parens)])
    #:static-infos ((#%call-result #,syntax-static-infos))
    (check-syntax who binding)
    (check-syntax who body)
    (check-syntax who static-infos)
    (syntax-parse binding
      [(parsed #:rhombus/bind _::binding-form)
       #`(parsed #:rhombus/annot
                 #,(annotation-binding-form
                    binding
                    (wrap-expression body)
                    (pack-static-infos who (unpack-term static-infos who #f))))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed binding form"
                                 "syntax object" binding)]))

  (define/arity (annot_meta.unpack_converter stx)
    #:static-infos ((#%call-result ((#%values (#,syntax-static-infos
                                               #,syntax-static-infos
                                               #,syntax-static-infos)))))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/annot a::annotation-binding-form)
       (values #'(parsed #:rhombus/bind a.binding)
               #'(parsed #:rhombus/expr a.body)
               (unpack-static-infos who #'a.static-infos))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "not a parsed converter annotation"
                                 "syntax object" stx)]))

  (define/arity (annot_meta.parse_to_packed_statinfo stx)
    #:static-infos ((#%call-result #,syntax-static-infos))
    (define group (unpack-group stx #f #f))
    (unless group
      (raise-argument-error* who rhombus-realm "Group" stx))
    (syntax-parse group
      [a::annotation
       #:with ab::annotation-binding-form #'a.parsed
       #'ab.static-infos]))
  )
