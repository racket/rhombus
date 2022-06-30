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

(provide annotation
         (for-syntax annotation_ct))

(define-simple-name-root annotation
  rule
  macro)

(begin-for-syntax
  (define-simple-name-root annotation_ct
    pack_predicate))

(define-operator-definition-transformer rule
  'rule
  in-annotation-space
  #'make-annotation-prefix-operator
  #'make-annotation-infix-operator
  #'annotation-prefix+infix-operator)

(define-operator-definition-transformer macro
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
  (syntax-parse (unpack-group form proc)
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
                               (unpack-tail new-tail proc)
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
                               (unpack-tail new-tail proc)
                               proc))))
  
(define-for-syntax (pack_predicate predicate [static-infos #'(parens)])
  #`(parsed #,(annotation-form (wrap-expression predicate)
                               (pack-static-infos (unpack-term static-infos 'annotation.pack_predicate)
                                                  'annotation.pack_predicate))))
