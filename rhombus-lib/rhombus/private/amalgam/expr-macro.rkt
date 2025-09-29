#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/transformer-result
                     enforest/transformer
                     enforest/proc-name
                     "srcloc.rkt"
                     "pack.rkt"
                     "pack-s-exp.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class)
                     "tail-returner.rkt"
                     "name-root.rkt"
                     "annotation-failure.rkt"
                     "define-arity.rkt"
                     (submod "syntax-object.rkt" for-quasiquote)
                     "call-result-key.rkt"
                     "values-key.rkt"
                     "maybe-key.rkt"
                     "macro-result.rkt"
                     "syntax-wrap.rkt"
                     (for-syntax racket/base))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         "macro-macro.rkt"
         "expression.rkt"
         "parse.rkt"
         "wrap-expression.rkt"
         "parse-meta.rkt"
         "parse-and-meta.rkt"
         "operator-compare.rkt"
         (submod "dot.rkt" for-syntax-meta))

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
     NameStart
     [parse_all expr_meta.parse_all]
     [parse_dot expr_meta.parse_dot]
     [pack_s_exp expr_meta.pack_s_exp]
     [pack_expr expr_meta.pack_expr]
     [pack_meta_expr expr_meta.pack_meta_expr]
     [pack_and_meta_expr expr_meta.pack_and_meta_expr]
     [relative_precedence expr_meta.relative_precedence]
     [ends_parse expr_meta.ends_parse])))

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
    NameStart in-expression-space
    AfterPrefixParsed :prefix-op+expression+tail
    AfterInfixParsed :infix-op+expression+tail))

(define-for-syntax (parsed-argument form)
  ;; use `rhombus-local-expand` to expose static information
  (define expr (rhombus-local-expand form))
  (relocate+reraw form #`(parsed #:rhombus/expr #,expr) #:prop-stx form))

(define-for-syntax (extract-expression form proc
                                       #:relocate [relocate-to/in #f]
                                       #:srcloc [loc #f])
  (define relocate-to (and relocate-to/in (maybe-respan relocate-to/in)))
  (define stx
    (syntax-parse form
      #:datum-literals (parsed group multi)
      [(multi (group (parsed #:rhombus/expr e))) #'e]
      [(group (parsed #:rhombus/expr e)) #'e]
      [(parsed #:rhombus/expr e) #'e]
      [_
       (define norm-form (normalize-result form proc))
       (define reloc-form (if relocate-to
                              (relocate+reraw relocate-to norm-form #:prop-stx norm-form)
                              norm-form))
       (syntax-parse (unpack-group reloc-form 'expression #f)
         [e::expression #'e.parsed])]))
  (if (or relocate-to loc)
      (relocate+reraw (or relocate-to loc) stx #:prop-stx stx)
      stx))

(define-for-syntax (normalize-result form proc)
  ;; make sure the result is a single group, and also strip away `multi`
  ;; so that a relocation (if any) is attached to te group, not a sequence
  ;; that will be immediately discarded
  (syntax-parse form
    #:datum-literals (multi)
    [(multi g) #'g]
    [(multi . _) (raise-bad-macro-result (proc-name proc)
                                         "single-group syntax object"
                                         form
                                         #:syntax-for? #f)]
    [_ form]))

(define-for-syntax empty-tail #'(multi)) ; recognize by `eq?`

(define-for-syntax (make-expression-infix-operator order prec protocol proc assc)
  (expression-infix-operator
   order
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form1 form2 stx)
         (extract-expression (check-expression-result
                              (proc (parsed-argument form1) (parsed-argument form2) stx)
                              proc)
                             proc
                             #:srcloc (datum->syntax #f (list form1 stx form2))))
       (lambda (form1 tail)
         (define-values (form new-tail)
           (tail-returner
            #:empty-tail empty-tail
            proc
            (syntax-parse tail
              [(head . tail) (proc (parsed-argument form1) (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-expression (check-expression-result form proc)
                                                       proc
                                                       #:relocate (and (eq? new-tail empty-tail)
                                                                       (datum->syntax #f (cons form1 tail)))
                                                       #:srcloc form)
                                   (unpack-tail new-tail proc #f)
                                   proc)))
   assc))

(define-for-syntax (make-expression-prefix-operator order prec protocol proc)
  (expression-prefix-operator
   order
   prec
   protocol
   (if (eq? protocol 'automatic)
       (lambda (form stx)
         (extract-expression (check-expression-result
                              (proc (parsed-argument form) stx)
                              proc)
                             proc
                             #:srcloc (datum->syntax #f (list stx form))))
       (lambda (tail)
         (define-values (form new-tail)
           (tail-returner
            #:empty-tail empty-tail
            proc
            (syntax-parse tail
              [(head . tail) (proc (pack-tail #'tail #:after #'head) #'head)])))
         (check-transformer-result (extract-expression (check-expression-result form proc)
                                                       proc
                                                       #:relocate (and (eq? new-tail empty-tail)
                                                                       tail)
                                                       #:srcloc form)
                                   (unpack-tail new-tail proc #f)
                                   proc)))))

(define-for-syntax (check-syntax who s)
  (unless (syntax*? s)
    (raise-annotation-failure who s "Syntax")))

(begin-for-syntax
  (define/arity (expr_meta.pack_s_exp orig-s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    #`(parsed
       #:rhombus/expr
       #,(pack-s-exp who orig-s)))

  (define/arity (expr_meta.pack_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (maybe-respan (unpack-group s who #f)))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression #,g)) #:prop-stx g))

  (define/arity (expr_meta.pack_meta_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (maybe-respan (unpack-group s who #f)))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression/meta #,g)) #:prop-stx g))

  (define/arity (expr_meta.pack_and_meta_expr s)
    #:static-infos ((#%call-result #,(get-syntax-static-infos)))
    (check-syntax who s)
    (define g (maybe-respan (unpack-group s who #f)))
    (relocate+reraw g #`(parsed #:rhombus/expr (rhombus-expression/both #,g)) #:prop-stx g))

  (define/arity (expr_meta.parse_all s)
    #:static-infos ((#%call-result ((#%values (#,(get-syntax-static-infos)
                                               #,(get-syntax-static-infos))))))
    (check-syntax who s)
    (syntax-parse (unpack-group s who #f)
      [e::expression
       (define-values (expr opaque)
         (syntax-local-expand-expression #'e.parsed))
       (let ([expr (maybe-respan expr)])
         (values (relocate+reraw expr #`(parsed #:rhombus/expr #,expr) #:prop-stx expr)
                 (relocate+reraw expr #`(parsed #:rhombus/expr #,opaque) #:prop-stx expr)))]))

  (define/arity (expr_meta.parse_dot form1 tail
                                     #:as_static [more-static? #f]
                                     #:disable_generic [no-generic? #t])
    #:static-infos ((#%call-result ((#%values (((#%maybe #,(get-syntax-static-infos)))
                                               ((#%maybe #,(get-syntax-static-infos))))))))
    (define-values (expr new-tail)
      (syntax-parse (unpack-term form1 who #f)
        #:datum-literals (parsed)
        [(parsed #:rhombus/expr v)
         (parse-dot-expr #'v (unpack-tail tail who #f)
                         #:as-static? more-static?
                         #:no-generic? no-generic?)]
        [_
         (raise-syntax-error who "not a parsed expression" form1)]))
    (if expr
        (let ([expr (maybe-respan expr)])
          (values (relocate+reraw expr #`(parsed #:rhombus/expr #,expr) #:prop-stx expr)
                  (pack-tail new-tail)))
        (values #f #f)))

  (define/arity (expr_meta.relative_precedence left-mode left-stx right-stx)
    (get-relative-precedence who left-mode left-stx right-stx
                             #f expression-relative-precedence))

  (define/arity (expr_meta.ends_parse left-mode left-stx tail)
    (ends-parse? who left-mode left-stx tail
                 #f
                 expression-relative-precedence
                 expression-infix-operator-ref)))
