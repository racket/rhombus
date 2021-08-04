#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     enforest/proc-name
                     enforest/transformer-result
                     "srcloc.rkt"
                     "tail.rkt")
         "lexicon.rkt"
         "definition.rkt"
         "expression.rkt"
         "expression+definition.rkt"
         "syntax.rkt"
         "binding.rkt"
         (rename-in "quasiquote.rkt"
                    [... rhombus...])
         (submod "quasiquote.rkt" convert)
         "parse.rkt"
         ;; for `matcher` and `binder`:
         (for-syntax "parse.rkt"))

(provide bind
         (for-syntax unpack_binding
                     pack_binding))

(define-syntax bind
  (simple-lexicon operator
                  macro
                  matcher
                  binder))

(define-syntax operator
  (make-operator-definition-transformer 'automatic
                                        in-binding-space
                                        #'make-binding-prefix-operator
                                        #'make-binding-infix-operator
                                        #'prefix+infix))

(define-syntax macro
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
     #`(parens (group b.arg-id)
               (group chain-to-matcher)
               (group chain-to-binder)
               (group (parsed (b.matcher-id b.binder-id b.data))))]))

(define-for-syntax (pack_binding stx)
  #`(parsed
     #,(syntax-parse stx
         #:datum-literals (parens group block)
         [(parens (group arg-id:identifier)
                  (group matcher-id:identifier)
                  (group binder-id:identifier)
                  (group data))
          (binding-form #'arg-id
                        #'matcher-id
                        #'binder-id
                        #'data)]
         [_ (raise-syntax-error 'pack_binding
                                "ill-formed unpacked binding"
                                stx)])))

(define-syntax matcher
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op parens group block)
        #:literals (? ¿)
        [(form-id (op ?) (parens (group builder-id:identifier
                                        (parens (group (op ¿) arg-id:identifier)
                                                data-pattern
                                                (group (op ¿) IF-id:identifier)
                                                (group (op ¿) success-id:identifier)
                                                (group (op ¿) fail-id:identifier))))
                  (block body ...))
         (define-values (converted-pattern idrs can-be-empty?) (convert-pattern #'data-pattern))
         (with-syntax ([((id id-ref) ...) idrs])
           (list
            #`(define-syntax (builder-id stx)
                (syntax-parse stx
                  [(_ arg-id data IF success fail)
                   (syntax-parse #'(group data)
                     [#,converted-pattern
                      (let ([id id-ref] ... [arg-id #'arg-id])
                        (let ([IF-id #'if-bridge])
                          (let ([success-id #'(parsed success)]
                                ;; putting `if-bridge` in `fail-id`
                                ;; helps make sure it's used correctly
                                [fail-id #'(parsed (if-bridge IF fail))])
                            (unwrap-block
                             (rhombus-block body ...)))))])]))))]))))

(define-syntax if-bridge
  ;; depending on `IF`, `if-bridge` will be used in an expression
  ;; or definition context
  (let ([parse (lambda (stx)
                 (syntax-parse stx
                   #:datum-literals (alts block parsed)
                   [(form-id e ... (alts (block success ...)
                                         (block . fail-case)))
                    (syntax-parse #'fail-case
                      #:datum-literals (group parsed)
                      #:literals (if-bridge)
                      [((group (parsed (if-bridge IF fail))))
                       #`(IF (rhombus-expression (group e ...))
                             (rhombus-body success ...)
                             fail)]
                      [_ (raise-syntax-error #f
                                             "not the given failure form in the failure branch"
                                             stx)])]))])
    (make-expression+definition-transformer
     (expression-transformer
      #'if-bridge
      (lambda (stx) (values (parse stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse stx)))))))

(define-syntax chain-to-matcher
  ;; depends on `IF` like `if-bridge` does
  (let ([parse (lambda (rhombus stx)
                 (syntax-parse stx
                   #:datum-literals (parsed group parens)
                   #:literals (if-bridge)
                   [(_ (parens (group arg-id:identifier)
                               (group (parsed (matcher-id binder-id data)))
                               (group IF-bridge)
                               (group success ...)
                               (group (parsed (if-bridge IF fail)))))
                    #:with rhombus rhombus
                    #'(matcher-id arg-id data IF (rhombus (group success ...)) fail)]))])
    (make-expression+definition-transformer
     (expression-transformer
      #'chain-to-matcher
      (lambda (stx) (values (parse #'rhombus-block stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse #'rhombus-body stx)))))))

(define-syntax chain-to-binder
  ;; depends on `IF` like `if-bridge` does
  (let ([parse (lambda (rhombus stx)
                 (syntax-parse stx
                   #:datum-literals (parsed group parens)
                   #:literals (if-bridge)
                   [(_ (parens (group arg-id:identifier)
                               (group (parsed (matcher-id binder-id data)))))
                    #:with rhombus rhombus
                    #'(binder-id arg-id data)]))])
    (make-expression+definition-transformer
     (expression-transformer
      #'chain-to-matcher
      (lambda (stx) (values (parse #'rhombus-block stx) #'())))
     (definition-transformer
       (lambda (stx) (list (parse #'rhombus-body stx)))))))

(define-syntax binder
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (op parens group block)
        #:literals (? ¿)
        [(form-id (op ?) (parens (group builder-id:identifier
                                        (parens (group (op ¿) arg-id:identifier)
                                                data-pattern)))
                  (block body ...))
         (define-values (converted-pattern idrs can-be-empty?) (convert-pattern #'data-pattern))
         (with-syntax ([((id id-ref) ...) idrs])
           (list
            #`(define-syntax (builder-id stx)
                (syntax-parse stx
                  [(_ arg-id data)
                   (syntax-parse #'(group data)
                     [#,converted-pattern
                      (let ([id id-ref] ... [arg-id #'arg-id])
                        (unwrap-block
                         (rhombus-block body ...)))])]))))]))))

(define-for-syntax (unwrap-block stx)
  (syntax-parse stx
    #:datum-literals (block)
    [(block g ...)
     #'(rhombus-body g ...)]))

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
