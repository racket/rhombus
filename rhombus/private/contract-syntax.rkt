#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/transformer-result
                     enforest/proc-name
                     "tail.rkt"
                     "static-info-pack.rkt")
         "definition.rkt"
         (submod "contract.rkt" for-struct)
         "syntax.rkt"
         "name-root.rkt"
         (for-syntax "name-root.rkt")
         "parse.rkt")

(provide (rename-out [rhombus-contract contract])
         (for-syntax contract_ct))

(define-syntax rhombus-contract
  (simple-name-root macro))

(begin-for-syntax
  (define-syntax contract_ct
    (simple-name-root pack_predicate)))

(define-syntax macro
  (make-operator-definition-transformer 'macro
                                        in-contract-space
                                        #'make-contract-prefix-operator
                                        #'make-contract-infix-operator
                                        #'contract-prefix+infix-operator))

(begin-for-syntax
  (struct contract-prefix+infix-operator (prefix infix)
    #:property prop:contract-prefix-operator (lambda (self) (contract-prefix+infix-operator-prefix self))
    #:property prop:contract-infix-operator (lambda (self) (contract-prefix+infix-operator-infix self))))

(define-for-syntax (parse-contract-macro-result form proc)
  (unless (syntax? form)
    (raise-result-error (proc-name proc) "syntax?" form))
  (syntax-parse #`(group #,form)
    [c::contract #'c.parsed]))

(define-for-syntax (make-contract-infix-operator name prec protocol proc assc)
  (contract-infix-operator
   name
   prec
   protocol
   (lambda (form1 tail)
     (define-values (form new-tail) (syntax-parse tail
                                      [(head . tail) (proc #`(parsed #,form1) (pack-tail #'tail) #'head)]))
     (check-transformer-result (parse-contract-macro-result form proc)
                               (unpack-tail new-tail proc)
                               proc))
   assc))

(define-for-syntax (make-contract-prefix-operator name prec protocol proc)
  (contract-prefix-operator
   name
   prec
   protocol
   (lambda (tail)
     (define-values (form new-tail) (syntax-parse tail
                                      [(head . tail) (proc (pack-tail #'tail) #'head)]))
     (check-transformer-result (parse-contract-macro-result form proc)
                               (unpack-tail new-tail proc)
                               proc))))
  
(define-for-syntax (pack_predicate predicate [static-infos #'(parens)])
  #`(parsed #,(contract-form #`(rhombus-expression (group #,predicate))
                             (pack-static-infos static-infos 'contract.pack_predicate))))
