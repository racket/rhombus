#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/transformer
                     "srcloc.rkt"
                     "pack.rkt"
                     "pack-s-exp.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "tail-returner.rkt"
                     "name-root.rkt"
                     "realm.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "values-key.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "macro-macro.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parse-meta.rkt"
         "parse-and-meta.rkt")

(provide (for-syntax (for-space rhombus/namespace
                                expr_meta)))

(module+ for-define
  (provide (for-syntax make-expression-infix-operator
                       make-expression-prefix-operator)))

(define+provide-space expr #f
  #:fields
  (macro))

(begin-for-syntax
  (define-name-root expr_meta
    #:fields
    (space
     Parsed
     AfterPrefixParsed
     AfterInfixParsed
     [parse_more expr_meta.parse_more]
     [parse_all expr_meta.parse_all]
     [pack_s_exp expr_meta.pack_s_exp]
     [pack_expr expr_meta.pack_expr]
     [pack_meta_expr expr_meta.pack_meta_expr]
     [pack_and_meta_expr expr_meta.pack_and_meta_expr])))

(define-for-syntax space
  (space-syntax #f))

(define-operator-definition-transformer macro
  'macro
  #f
  #'make-expression-prefix-operator
  #'make-expression-infix-operator
  #'expression-prefix+infix-operator)

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :expression #:rhombus/expr
    AfterPrefixParsed :prefix-op+expression+tail
    AfterInfixParsed :infix-op+expression+tail))

(define-for-syntax (parsed-argument form)
  ;; use `rhombus-local-expand` to expose static information
  (define loc (maybe-respan form))
  (define expr (relocate+reraw loc (rhombus-local-expand form)))
  (relocate+reraw expr #`(parsed #:rhombus/expr #,expr)))

(define-for-syntax (make-expression-infix-operator prec protocol proc assc)
  (expression-infix-operator
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form1) (parsed-argument form2) stx)
                           proc)
                          #:srcloc (datum->syntax #f (list form1 stx form2))))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-expression-prefix-operator prec protocol proc)
  (expression-prefix-operator
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form) stx)
                           proc)
                          #:srcloc (datum->syntax #f (list stx form))))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))))

(define-for-syntax (check-syntax who s)
  (unless (syntax? s)
    (raise-argument-error* who rhombus-realm "Syntax" s)))

(begin-for-syntax
  (define/arity (expr_meta.pack_s_exp orig-s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    #`(parsed
       #:rhombus/expr
       #,(pack-s-exp who orig-s)))

  (define/arity (expr_meta.pack_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (unpack-group s who #f))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression #,g))))

  (define/arity (expr_meta.pack_meta_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (unpack-group s who #f))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression/meta #,g))))

  (define/arity (expr_meta.pack_and_meta_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (unpack-group s who #f))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression/both #,g))))

  (define/arity (expr_meta.parse_more s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (syntax-parse (unpack-group s who #f)
      [e::expression
       (define new-e (rhombus-local-expand #'e.parsed))
       (relocate+reraw new-e #`(parsed #:rhombus/expr #,new-e))]))

  (define/arity (expr_meta.parse_all s)
    #:static-infos ((#%call-result ((#%values (#,(get-syntax-static-infos)
                                               #,(get-syntax-static-infos))))))
    (check-syntax who s)
    (syntax-parse (unpack-group s who #f)
      [e::expression
       (define-values (expr opaque)
         (syntax-local-expand-expression #'e.parsed))
       (values (relocate+reraw expr #`(parsed #:rhombus/expr
                                              #,expr))
               (relocate+reraw expr #`(parsed #:rhombus/expr
                                              #,opaque)))]))
  )
