#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     "name-path-op.rkt")
         syntax/parse/pre
         "pack.rkt"
         "syntax-class.rkt"
         (submod "syntax-class.rkt" for-quasiquote)
         (only-in "annotation.rkt"
                  ::)
         "pattern-variable.rkt"
         "syntax-binding.rkt"
         "name-root-ref.rkt")

(provide (for-space rhombus/syntax_binding
                    _
                    #%parens
                    #%quotes
                    ::))

(define-syntax-binding-syntax _
  (syntax-binding-prefix-operator
   #'_
   null
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values #`(#,(syntax/loc #'form-id _) () () ())
                #'tail)]))))

(define-syntax-binding-syntax #%parens
  (syntax-binding-prefix-operator
   #'#%parens
   null
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ (parens g::syntax-binding) . tail)
        (values #'g.parsed
                #'tail)]))))

(define-syntax-binding-syntax #%quotes
  (syntax-binding-prefix-operator
   #'#%quotes
   null
   'macro
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (quotes group)
       [(_ (quotes (group t)) . tail)
        (values (if (eq? (current-syntax-binding-kind) 'term)
                    #'((~datum t) () () ())
                    #'#f)
                #'tail)]))))

(define-syntax-binding-syntax ::
  (syntax-binding-infix-operator
   #'::
   null
   'macro
   (lambda (form1 stx)
     (unless (identifier? form1)
       (raise-syntax-error #f
                           "preceding term must be an identifier"
                           (syntax-parse stx
                             [(colons . _) #'colons])))
     (syntax-parse stx
       [(_ . rest)
        #:with (~var stx-class-hier (:hier-name-seq in-syntax-class-space name-path-op name-root-ref)) #'rest
        #:with stx-class #'stx-class-hier.name
        #:with tail #'stx-class-hier.tail
        (values
         (with-syntax ([id form1])
           (define rsc (syntax-local-value (in-syntax-class-space #'stx-class) (lambda () #f)))
           (define (compat pack* unpack*)
             (define sc (rhombus-syntax-class-class rsc))
             (define temp0-id (car (generate-temporaries (list #'id))))
             (define temp-id (car (generate-temporaries (list #'id))))
             (define-values (attribute-bindings attribute-mappings)
               (for/lists (bindings mappings)
                   ([name+depth (in-list (rhombus-syntax-class-attributes rsc))]
                    [temp-attr (in-list (generate-temporaries (map car (rhombus-syntax-class-attributes rsc))))])
                 (define name (car name+depth))
                 (define depth (cdr name+depth))
                 (define id-with-attr
                   (datum->syntax temp0-id (string->symbol (format "~a.~a" (syntax-e temp0-id) name))))
                 (values #`[#,temp-attr (pack-term*
                                         (syntax #,(let loop ([t id-with-attr] [depth depth])
                                                     (if (zero? depth)
                                                         t
                                                         (loop #`(#,t #,(quote-syntax ...)) (sub1 depth)))))
                                         #,depth)]
                         (cons name (cons temp-attr depth)))))
             (define pack-depth (if (rhombus-syntax-class-splicing? rsc) 1 0))
             #`[#,(if sc
                      #`(~var #,temp0-id #,sc)
                      temp0-id)
                #,(cons #`[#,temp-id (#,pack* (syntax #,temp0-id) #,pack-depth)] attribute-bindings)
                #,(list #`[id (make-pattern-variable-syntax
                               (quote-syntax id)
                               (quote-syntax #,temp-id)
                               (quote-syntax #,unpack*)
                               #,pack-depth
                               #,(rhombus-syntax-class-splicing? rsc)
                               (hasheq #,@(apply append (for/list ([b (in-list attribute-mappings)])
                                                          (list #`(quote #,(car b))
                                                                #`(syntax-class-attribute (quote-syntax #,(cadr b))
                                                                                          #,(cddr b)))))))])
                #,(list (list #'id temp-id pack-depth unpack*))])
           (define (incompat)
             (raise-syntax-error #f
                                 "syntax class incompatible with this context"
                                 #'stx-class))
           (define kind (current-syntax-binding-kind))
           (cond
             [(not (rhombus-syntax-class? rsc))
              (raise-syntax-error #f
                                  "not bound as a syntax class"
                                  #'stx-class)]
             [(eq? (rhombus-syntax-class-kind rsc) 'term)
              (cond
                [(not (eq? kind 'term)) #'#f]
                [else (compat #'pack-term* #'unpack-term*)])]
             [(eq? (rhombus-syntax-class-kind rsc) 'group)
              (cond
                [(eq? kind 'term) (incompat)]
                [(not (eq? kind 'group)) #'#f]
                [else (compat #'pack-group* #'unpack-group* )])]
             [(eq? (rhombus-syntax-class-kind rsc) 'multi)
              (cond
                [(or (eq? kind 'multi) (eq? kind 'block))
                 (compat #'pack-tagged-multi* #'unpack-multi-as-term*)]
                [else (incompat)])]
             [(eq? (rhombus-syntax-class-kind rsc) 'block)
              (cond
                [(eq? kind 'block)
                 (compat #'pack-block* #'unpack-multi-as-term*)]
                [else (incompat)])]
             [else
              (error "unrecognized kind" kind)]))
         #'tail)]))
   'none))

