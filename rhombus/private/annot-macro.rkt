#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/proc-name
                     "pack.rkt"
                     "static-info-pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class))
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "space.rkt"
         "macro-macro.rkt"
         "wrap-expression.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         "parse.rkt")

(provide annot
         (for-syntax annot_meta))

(module+ for-class
  (provide (for-syntax make-annotation-prefix-operator)))

(define-name-root annot
  #:root (space-syntax rhombus/annot)
  #:fields
  (macro
   only))

(define-name-root only
  #:fields
  ([macro macro-only]))

(begin-for-syntax
  (define-name-root annot_meta
    #:fields
    (pack_predicate
     Group
     AfterPrefixGroup
     AfterInfixGroup)))

(define-operator-definition-transformer+only macro macro-only
  'macro
  rhombus/annot
  #'make-annotation-prefix-operator
  #'make-annotation-infix-operator
  #'annotation-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Group :annotation
    AfterPrefixGroup :prefix-op+annotation+tail
    AfterInfixGroup :infix-op+annotation+tail))

(begin-for-syntax
  (struct annotation-prefix+infix-operator (prefix infix)
    #:property prop:annotation-prefix-operator (lambda (self) (annotation-prefix+infix-operator-prefix self))
    #:property prop:annotation-infix-operator (lambda (self) (annotation-prefix+infix-operator-infix self))))

(define-for-syntax (parse-annotation-macro-result form proc)
  (unless (syntax? form)
    (raise-result-error (proc-name proc) "syntax?" form))
  (syntax-parse (unpack-group form proc #f)
    [c::annotation #'c.parsed]))

(define-for-syntax (make-annotation-infix-operator name prec protocol proc assc)
  (annotation-infix-operator
   name
   prec
   protocol
   (lambda (form1 tail)
     (define-values (form new-tail) (syntax-parse tail
                                      [(head . tail) (proc #`(parsed #,form1) (pack-tail #'tail) #'head)]))
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
     (define-values (form new-tail) (syntax-parse tail
                                      [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (check-transformer-result (parse-annotation-macro-result form proc)
                               (unpack-tail new-tail proc #f)
                               proc))))
  
(define-for-syntax (pack_predicate predicate [static-infos #'(parens)])
  #`(parsed #,(annotation-form (wrap-expression predicate)
                               (pack-static-infos (unpack-term static-infos 'annot.pack_predicate #f)
                                                  'annot.pack_predicate))))
