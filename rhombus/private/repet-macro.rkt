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
                     (submod "syntax-class-primitive.rkt" for-syntax-class))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "repetition.rkt"
         "space.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "parse.rkt")

(define+provide-space repet rhombus/repet
  #:fields
  (macro))

(provide (for-syntax (for-space rhombus/namespace
                                repet_meta)))

(begin-for-syntax
  (define-name-root repet_meta
    #:fields
    (space
     pack_list
     unpack_list
     Parsed
     AfterPrefixParsed
     AfterInfixParsed)))

(define-operator-definition-transformer macro
  'macro
  rhombus/repet
  #'make-repetition-prefix-operator
  #'make-repetition-infix-operator
  #'prefix+infix)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :repetition #:rhombus/repet
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

(define-for-syntax (make-repetition-infix-operator name prec protocol proc assc)
  (repetition-infix-operator
   name
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

(define-for-syntax (make-repetition-prefix-operator name prec protocol proc)
  (repetition-prefix-operator
   name
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

(define-for-syntax (pack_list stx)
  (syntax-parse (unpack-term stx 'repet_meta.pack #f)
    #:datum-literals (group)
    [(parens (group orig-form ...)
             (group name:identifier)
             seq-expr
             (group bind-depth:exact-nonnegative-integer)
             (group use-depth:exact-nonnegative-integer)
             (group element-static-infos)
             (group immediate?:boolean))
     (wrap-parsed
      (make-repetition-info #'(orig-form ...)
                            #'name
                            #'(rhombus-expression seq-expr)
                            #'bind-depth
                            #'use-depth
                            (pack-static-infos #'element-static-infos 'repet_meta.pack)
                            #'immediate?))]
    [_ (raise-syntax-error 'repet_meta.pack_info
                           "ill-formed unpacked repetiton info"
                           stx)]))

(define-for-syntax (unpack_list stx)
  (syntax-parse (unpack-term stx 'repet_meta.unpack #f)
    [((~datum parsed) #:rhombus/repet r::repetition-info)
     (pack-term
      #`(parens (group . r.rep-expr)
                (group r.name)
                (group (parsed #:rhombus/expr r.seq-expr))
                (group r.bind-depth)
                (group r.use-depth)
                (group #,(unpack-static-infos #'r.element-static-infos))
                (group r.immediate?)))]))
