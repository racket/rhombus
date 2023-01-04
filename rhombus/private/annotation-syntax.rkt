#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer-result
                     enforest/proc-name
                     "pack.rkt"
                     "static-info-pack.rkt")
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "syntax.rkt"
         "wrap-expression.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         "parse.rkt")

(provide annot
         (for-syntax annot_meta))

(module+ for-class
  (provide (for-syntax make-annotation-prefix-operator)))

(define-simple-name-root annot
  rule
  macro
  only)

(define-name-root only
  #:fields
  ([macro macro-only]
   [rule rule-only]))

(begin-for-syntax
  (define-simple-name-root annot_meta
    pack_predicate))

(define-operator-definition-transformer+only rule rule-only
  'rule
  in-annotation-space
  #'make-annotation-prefix-operator
  #'make-annotation-infix-operator
  #'annotation-prefix+infix-operator)

(define-operator-definition-transformer+only macro macro-only
  'macro
  in-annotation-space
  #'make-annotation-prefix-operator
  #'make-annotation-infix-operator
  #'annotation-prefix+infix-operator)

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
