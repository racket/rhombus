#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/proc-name
                     enforest/transformer-result
                     "pack.rkt"
                     "static-info-pack.rkt"
                     "macro-result.rkt"
                     "tail-returner.rkt"
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "realm.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "treelist.rkt"
         "space-provide.rkt"
         "repetition.rkt"
         "space.rkt"
         "macro-macro.rkt"
         "parse.rkt"
         "parens.rkt"
         "wrap-expression.rkt")

(define+provide-space repet rhombus/repet
  #:fields
  (macro))

(provide (for-syntax (for-space rhombus/namespace
                                repet_meta)))

(begin-for-syntax
  (define-name-root repet_meta
    #:fields
    (space
     [pack_list repet_meta.pack_list]
     [unpack_list repet_meta.unpack_list]
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     NameStart)))

(define-operator-definition-transformer macro
  'macro
  rhombus/repet
  #'make-repetition-prefix-operator
  #'make-repetition-infix-operator
  #'prefix+infix)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :repetition #:rhombus/repet
    NameStart in-repet-space
    AfterPrefixParsed :prefix-op+repetition+tail
    AfterInfixParsed :infix-op+repetition+tail))

(define-for-syntax space
  (space-syntax rhombus/repet))

(begin-for-syntax
  (struct prefix+infix (prefix infix)
    #:property prop:repetition-prefix-operator (lambda (self) (prefix+infix-prefix self))
    #:property prop:repetition-infix-operator (lambda (self) (prefix+infix-infix self))))

(define-for-syntax (extract-repetition form proc)
  (syntax-parse (if (syntax? form)
                    (unpack-group form proc #f)
                    #'#f)
    [b::repetition #'b.parsed]
    [_ (raise-bad-macro-result (proc-name proc) "repetition" form)]))

(define-for-syntax (wrap-parsed stx)
  #`(parsed #:rhombus/repet #,stx))

(define-for-syntax (make-repetition-infix-operator prec protocol proc assc)
  (repetition-infix-operator
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (wrap-parsed form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-repetition form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form1 form2 stx)
         (extract-repetition (proc (wrap-parsed form1) (wrap-parsed form2) stx)
                          proc)))
   assc))

(define-for-syntax (make-repetition-prefix-operator prec protocol proc)
  (repetition-prefix-operator
   prec
   protocol
   (if (eq? protocol 'macro)
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-repetition form proc)
                                   (unpack-tail new-tail proc #f)
                                   proc))
       (lambda (form stx)
         (extract-repetition (proc (wrap-parsed form) stx)
                             proc)))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-annotation-failure who s "Syntax")))

(begin-for-syntax
  (define/arity (repet_meta.pack_list stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (group)
      [(_::parens (group orig-form ...)
                  seq-expr
                  (group bind-depth:exact-nonnegative-integer)
                  (group use-depth:exact-nonnegative-integer)
                  (group element-static-infos))
       #:with (elem) (generate-temporaries '(elem))
       (wrap-parsed
        (if (= 0 (syntax-e #'bind-depth))
            (make-repetition-info #'(orig-form ...)
                                  #'()
                                  (wrap-expression #'seq-expr)
                                  (pack-static-infos who #'element-static-infos)
                                  #'use-depth)
            (make-repetition-info #'(orig-form ...)
                                  #`(([(elem) (in-treelist #,(wrap-expression #'seq-expr))])
                                     #,@(for/list ([i (in-range (sub1 (syntax-e #'bind-depth)))])
                                          #`([(elem) (in-treelist elem)])))
                                  #'elem
                                  (pack-static-infos who #'element-static-infos)
                                  #'use-depth)))]
      [_ (raise-arguments-error* who rhombus-realm
                                 "ill-formed unpacked repetiton info"
                                 "syntax object" stx)]))

  (define/arity (repet_meta.unpack_list stx)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who stx)
    (syntax-parse (unpack-term stx who #f)
      #:datum-literals (parsed)
      [(parsed #:rhombus/repet r::repetition-info)
       (define depth (length (syntax->list #'r.for-clausess)))
       (pack-term
        #`(parens (group . r.rep-expr)
                  (group (parsed #:rhombus/expr #,(repetition-as-nested-lists #'r depth #'for/treelist)))
                  (group #,depth)
                  (group r.used-depth)
                  (group #,(unpack-static-infos who #'r.element-static-infos))))]))
  )
