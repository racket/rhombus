#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer-result
                     enforest/proc-name
                     "tail.rkt"
                     "static-info-pack.rkt")
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "syntax.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         "parse.rkt")

(provide annotation
         (for-syntax annotation_ct))

(define-syntax annotation
  (simple-name-root macro))

(begin-for-syntax
  (define-syntax annotation_ct
    (simple-name-root pack_predicate)))

(define-syntax macro
  (make-operator-definition-transformer 'macro
                                        in-annotation-space
                                        #'make-annotation-prefix-operator
                                        #'make-annotation-infix-operator
                                        #'annotation-prefix+infix-operator))

(begin-for-syntax
  (struct annotation-prefix+infix-operator (prefix infix)
    #:property prop:annotation-prefix-operator (lambda (self) (annotation-prefix+infix-operator-prefix self))
    #:property prop:annotation-infix-operator (lambda (self) (annotation-prefix+infix-operator-infix self))))

(define-for-syntax (parse-annotation-macro-result form proc)
  (unless (syntax? form)
    (raise-result-error (proc-name proc) "syntax?" form))
  (syntax-parse #`(group #,form)
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
  #`(parsed #,(annotation-form #`(rhombus-expression (group #,predicate))
                               (pack-static-infos static-infos 'annotation.pack_predicate))))
