#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     "srcloc.rkt"
                     "pack.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     (for-syntax racket/base)
                     "tail-returner.rkt"
                     "realm.rkt")
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "name-root.rkt"
         "macro-macro.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         (for-syntax "name-root.rkt"))

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
     parse_more
     pack_s_exp
     pack_expr)))

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
    Parsed :expression
    AfterPrefixParsed :prefix-op+expression+tail
    AfterInfixParsed :infix-op+expression+tail))

(define-for-syntax (parsed-argument form)
  ;; use `rhombus-local-expand` to expose static information
  (define loc (maybe-respan form))
  (relocate loc #`(parsed #,(relocate loc (rhombus-local-expand form)))))

(define-for-syntax (make-expression-infix-operator name prec protocol proc assc)
  (expression-infix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form1) (parsed-argument form2) stx)
                           proc)
                          #:srcloc (span-srcloc form1 form2)))
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

(define-for-syntax (make-expression-prefix-operator name prec protocol proc)
  (expression-prefix-operator
   name
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (wrap-expression (check-expression-result
                           (proc (parsed-argument form) stx)
                           proc)
                          #:srcloc (span-srcloc stx form)))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (wrap-expression (check-expression-result form proc))
                                   (unpack-tail new-tail proc #f)
                                   proc)))))

(define-for-syntax (pack_s_exp orig-s)
  #`(parsed
     #,(let loop ([s orig-s])
         (cond
           [(syntax? s)
            (define stx (unpack-term s 'expr.pack_s_exp #f))
            (syntax-parse stx
              #:datum-literals (parsed)
              [(parsed e) #'e]
              [_ stx])]
           [(list? s)
            (for/list ([e (in-list s)])
              (loop e))]
           [else
            (raise-arguments-error* 'expr.pack_s_exp rhombus-realm
                                    "not a list nesting of syntax objects"
                                    "value" orig-s)]))))

(define-for-syntax (pack_expr s)
  (unless (syntax? s)
    (raise-argument-error* 'expr.pack_expr rhombus-realm "Syntax" s))
  #`(parsed (rhombus-expression #,(unpack-group s 'expr.pack_expr #f))))

(define-for-syntax (parse_more s)
  (syntax-parse (unpack-group s 'expr_meta.parse_more #f)
    [e::expression #`(parsed #,(rhombus-local-expand #'e.parsed))]))
