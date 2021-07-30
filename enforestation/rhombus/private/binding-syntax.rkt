#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/proc-name
                     enforest/transformer-result
                     "srcloc.rkt"
                     "tail.rkt")
         "definition.rkt"
         "syntax.rkt"
         "binding.rkt"
         "parse.rkt")

(provide binding_operator
         binding_macro
         (for-syntax unpack_binding
                     pack_binding))

(define-syntax binding_operator
  (make-operator-definition-transformer 'automatic
                                        in-binding-space
                                        #'make-binding-prefix-operator
                                        #'make-binding-infix-operator
                                        #'prefix+infix))

(define-syntax binding_macro
  (make-operator-definition-transformer 'macro
                                        in-binding-space
                                        #'make-binding-prefix-operator
                                        #'make-binding-infix-operator
                                        #'prefix+infix))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:binding-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:binding-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (unpack_binding stx)
  (syntax-parse stx
    [((~datum parsed) b::binding-form)
     #`(parens (group (parens (group . b.var-ids)))
               (group (parsed b.check-proc-expr))
               (group (block (group (parsed b.post-defn)))))]))

(define-for-syntax (pack_binding stx)
  #`(parsed
     #,(syntax-parse stx
         #:datum-literals (parens group block)
         [(parens (group (parens (group id:identifier ...)))
                  (group e ...)
                  (group (block (group defn ...) ...)))
          (binding-form #'(id ...)
                        #'(rhombus-expression (group e ...))
                        #'(begin (rhombus-definition (group defn ...))
                                 ...))])))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #,stx))

(define-for-syntax (extract-binding form proc)
  (syntax-parse (if (syntax? form) #`(group #,form) #'#f)
    [b::binding #'b.parsed]
    [_ (raise-result-error (proc-name proc) "binding?" form)]))

(define-for-syntax (make-binding-infix-operator name prec protocol proc assc)
  (binding-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (form1 tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc (wrap-parsed form1) (pack-tail #'tail) #'head)]))
         (check-transformer-result (extract-binding form proc)
                                   (unpack-tail new-tail proc)
                                   proc))
       (lambda (form1 form2 stx)
         (extract-binding (proc (wrap-parsed form1) (wrap-parsed form2) stx)
                          proc)))
   assc))

(define-for-syntax (make-binding-prefix-operator name prec protocol proc)
  (binding-prefix-operator
   name
   prec
   protocol 
   (if (eq? protocol 'macro)
       (lambda (tail)
         (define-values (form new-tail) (syntax-parse tail
                                          [(head . tail) (proc (pack-tail #'tail) #'head)]))
         (check-transformer-result (extract-binding form proc)
                                   (unpack-tail new-tail proc)
                                   proc))
       (lambda (form stx)
         (extract-binding (proc (wrap-parsed form) stx)
                          proc)))))
