#lang racket/base
(require syntax/parse/pre
         (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         (submod "quasiquote.rkt" convert)
         "quasiquote.rkt"
         (only-in "ellipsis.rkt"
                  [... rhombus...])
         "parse.rkt"
         "srcloc.rkt"
         "binding.rkt"
         "op-literal.rkt"
         "pack.rkt"
         "entry-point.rkt"
         (only-in "static-info.rkt"
                  in-static-info-space
                  make-static-infos)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax parse-operator-definition-rhs
                     parse-operator-definitions-rhs
                     parse-transformer-definition-rhs
                     parse-transformer-definition-sequence-rhs))

(begin-for-syntax
  (struct parsed (fixity name opts-stx prec-stx assc-stx parsed-right?
                         ;; implementation is function stx if `parsed-right?`,
                         ;; or a clause over #'self and maybe #'left otherwise
                         impl)))

;; finish parsing one case (possibly the only case) in a macro definition,
;; now that we're in the right phase for the right-hand side of the definition
(define-for-syntax (parse-one-macro-definition pre-parsed adjustments)
  (define kind (syntax-parse pre-parsed
                 [(_ _ _ kind . _) (syntax-e #'kind)]))
  (define (macro-clause self-id left-ids tail-pattern rhs)
    (define-values (pattern idrs sidrs vars can-be-empty?)
      (if (eq? kind 'rule)
          (convert-pattern #`(group (op $) _ #,@tail-pattern (op $) tail (op rhombus...))
                           #:splice? #t
                           #:splice-pattern values)
          (convert-pattern #`(group (op $) _ . #,tail-pattern)
                           #:as-tail? #t
                           #:splice? #t
                           #:splice-pattern values)))
    (with-syntax ([((id id-ref) ...) idrs]
                  [((sid sid-ref) ...) sidrs]
                  [(left-id ...) left-ids])
      (define body
        (if (eq? kind 'rule)
            (let ([ids (cons self-id (append left-ids (syntax->list #'(id ... sid ...))))])
              #`(values #,(convert-rule-template rhs ids)
                        (tail-rule-template (multi (group (op $) tail (op rhombus...))))))
            #`(rhombus-body-expression #,rhs)))
      #`[#,pattern
         (let ([id id-ref] ... [#,self-id self] [left-id left] ...)
           (let-syntax ([sid sid-ref] ...)
             #,body))]))
  (define (convert-rule-template block ids)
    (syntax-parse block
      #:datum-literals (block group quotes op)
      [(block (group (quotes template)))
       ;; delay further conversion until after pattern variables are bound
       #`(rule-template template #,ids)]
      [(block (group e)) (raise-syntax-error 'template "invalid result template" #'e)]))
  (define (extract-pattern-id tail-pattern)
    (syntax-parse tail-pattern
      #:datum-literals (op)
      [((op (~var _ (:$ in-binding-space))) id:identifier) #'id]))
  (syntax-parse pre-parsed
    #:datum-literals (pre-parsed infix prefix nofix)
    ;; infix protocol
    [(pre-parsed name
                 infix
                 _
                 opt
                 prec
                 assc
                 parsed-right?
                 [tail-pattern
                  self-id
                  left
                  (tag rhs ...)])
     (parsed 'infix
             #'name
             #'opt
             #'prec
             #'assc
             (syntax-e #'parsed-right?)
             (cond
               [(syntax-e #'parsed-right?)
                (define right-id (extract-pattern-id #'tail-pattern))
                (define extra-args (entry-point-adjustments-prefix-arguments adjustments))
                #`(lambda (#,@extra-args left #,right-id self-id)
                    #,(adjust-result
                       adjustments
                       1
                       (if (eq? kind 'rule)
                           (convert-rule-template #'(tag rhs ...)
                                                  (list #'left right-id #'self-id))
                           #`(rhombus-body-expression (tag rhs ...)))))]
               [else
                (macro-clause #'self-id (list #'left)
                              #'tail-pattern
                              #'(tag rhs ...))]))]
    ;; prefix protocol
    [(pre-parsed name
                 prefix
                 _
                 opt
                 prec
                 #f
                 parsed-right?
                 [tail-pattern
                  self-id
                  (tag rhs ...)])
     (parsed 'prefix
             #'name
             #'opt
             #'prec
             #f
             (syntax-e #'parsed-right?)
             (cond
               [(syntax-e #'parsed-right?)
                (define arg-id (extract-pattern-id #'tail-pattern))
                (define extra-args (entry-point-adjustments-prefix-arguments adjustments))
                #`(lambda (#,@extra-args #,arg-id self-id)
                    #,(adjust-result
                       adjustments
                       1
                       (if (eq? kind 'rule)
                           (convert-rule-template #'(tag rhs ...)
                                                  (list arg-id #'opt-self-id))
                           #`(rhombus-body-expression (tag rhs ...)))))]
               [else
                (macro-clause #'self-id '()
                              #'tail-pattern
                              #'(tag rhs ...))]))]
    ;; nofix protocol
    [(pre-parsed name
                 nofix
                 _
                 opt
                 prec
                 #f
                 #f
                 [self-id
                  (tag rhs ...)])
     (parsed 'prefix
             #'name
             #'opt
             #'prec
             #f
             #f
             #`[_ (let ([self-id self])
                    (values #,(if (eq? kind 'rule)
                                  (convert-rule-template #'(tag rhs ...)
                                                         (list #'self-id))
                                  #`(rhombus-body-at tag rhs ...))
                            tail))])]))

(define-syntax (rule-template stx)
  (syntax-parse stx
    [(_ template ids)
     (let ([ids (syntax->list #'ids)])
       (convert-template #'(multi template)
                         #:rhombus-expression #'rhombus-expression
                         #:check-escape (lambda (e)
                                          (unless (or (and (identifier? e)
                                                           (for/or ([id (in-list ids)])
                                                             (free-identifier=? e id)))
                                                      (syntax-parse e
                                                        #:datum-literals (group parens quotes op)
                                                        [(parens (group (quotes (group (op _))))) #t]
                                                        [(quotes (group (op _))) #t]
                                                        [else #f]))
                                            (raise-syntax-error 'template
                                                                (string-append
                                                                 "expected an identifier bound by the pattern\n"
                                                                 " or a literal-operator syntax object")
                                                                e)))))]))

(define-syntax (tail-rule-template stx)
  (syntax-parse stx
    [(_ template)
     (convert-template #'template)]))

;; combine previously parsed cases (possibly the only case) in a macro
;; definition that are all either prefix or infix
(define-for-syntax (build-cases ps prefix? make-id adjustments)
  (define p (car ps))
  #`(#,make-id
     (quote-syntax #,(parsed-name p))
     #,(parsed-prec-stx p)
     #,(if (parsed-parsed-right? p)
           #''automatic
           #''macro)
     (let ([#,(parsed-name p)
            #,(if (parsed-parsed-right? p)
                  (parsed-impl p)
                  (let ([extra-args (entry-point-adjustments-prefix-arguments adjustments)])
                    #`(lambda (#,@extra-args #,@(if prefix? '() (list #'left)) tail self)
                        #,(adjust-result
                           adjustments
                           1
                           #`(syntax-parse (insert-multi-front-group self tail)
                               #,@(map parsed-impl ps))))))])
       #,(parsed-name p))
     #,@(if prefix?
            '()
            (list (parsed-assc-stx p)))))

;; single-case macro definition:
(define-for-syntax (parse-operator-definition-rhs pre-parsed
                                                  make-prefix-id make-infix-id
                                                  #:adjustments [adjustments no-adjustments])
  (define p (parse-one-macro-definition pre-parsed adjustments))
  (define op (parsed-name p))
  (define prefix? (eq? 'prefix (parsed-fixity p)))
  (define make-id (if prefix? make-prefix-id make-infix-id))
  (build-cases (list p) prefix? make-id adjustments))

;; multi-case macro definition:
(define-for-syntax (parse-operator-definitions-rhs orig-stx pre-parseds
                                                   make-prefix-id make-infix-id prefix+infix-id
                                                   #:adjustments [adjustments no-adjustments])
  (define ps (map (lambda (p) (parse-one-macro-definition p adjustments)) pre-parseds))
  (define prefixes (for/list ([p (in-list ps)] #:when (eq? 'prefix (parsed-fixity p))) p))
  (define infixes (for/list ([p (in-list ps)] #:when (eq? 'infix (parsed-fixity p))) p))
  (define (check-fixity-consistent what options ps)
    (unless ((length ps) . < . 2)
      (for ([p (in-list ps)]
            [i (in-naturals)])
        (when (parsed-parsed-right? p)
          (raise-syntax-error #f
                              (format "multiple ~a cases not allowed with a 'parsed_right' case"
                                      what)
                              orig-stx))
        (unless (zero? i)
          (when (for*/or ([d (syntax->list (parsed-opts-stx p))]
                          [d (in-list (or (syntax->list d) (list d)))])
                  (and (keyword? (syntax-e d))
                       (not (eq? '#:op_stx (syntax-e d)))))
            (raise-syntax-error #f
                                (format "~a options not allowed after first ~a case"
                                        options what)
                                orig-stx))))))
  (check-fixity-consistent "prefix" "precedence" prefixes)
  (check-fixity-consistent "infix" "precedence and associativity" infixes)
  (cond
    [(null? prefixes) (build-cases infixes #f make-infix-id adjustments)]
    [(null? infixes) (build-cases prefixes #t make-prefix-id adjustments)]
    [else #`(#,prefix+infix-id
             #,(build-cases prefixes #t make-prefix-id adjustments)
             #,(build-cases infixes #f make-infix-id adjustments))]))

(define-for-syntax (adjust-result adjustments arity b)
  ((entry-point-adjustments-wrap-body adjustments) arity b))

;; ----------------------------------------

(define-for-syntax (parse-transformer-definition-rhs pre-parsed self-id extra-id
                                                     make-transformer-id
                                                     extra-static-infos-stx
                                                     #:tail-ids [tail-ids '()]
                                                     #:wrap-for-tail [wrap-for-tail values])
  (syntax-parse pre-parsed
    #:datum-literals (pre-parsed)
    [(pre-parsed id
                 tail-pattern
                 rhs)
     (define-values (pattern idrs sidrs vars can-be-empty?) (convert-pattern #`(group (op $) _ . tail-pattern)
                                                                             #:as-tail? #t
                                                                             #:splice? #t
                                                                             #:splice-pattern values))
     (with-syntax ([((p-id id-ref) ...) idrs]
                   [((s-id sid-ref) ...) sidrs] )
       #`(#,make-transformer-id
          (let ([id (lambda (tail #,@tail-ids self #,@(if (syntax-e extra-id) (list #'extra) null))
                      (define #,self-id self)
                      (define-syntax #,(in-static-info-space self-id) (make-static-infos syntax-static-infos))
                      #,@(if (syntax-e extra-id)
                             #`((define #,extra-id extra)
                                (define-syntax #,(in-static-info-space extra-id) (make-static-infos #,extra-static-infos-stx)))
                             null)
                      (syntax-parse (insert-multi-front-group #,self-id tail)
                        [#,pattern
                         (let ([p-id id-ref] ...)
                           (let-syntax ([s-id sid-ref] ...)
                             #,(wrap-for-tail
                                #`(rhombus-body-expression rhs))))]))])
            id)))]))

(define-for-syntax (parse-transformer-definition-sequence-rhs pre-parsed self-id
                                                              make-transformer-id
                                                              gs-stx)
  (parse-transformer-definition-rhs pre-parsed self-id #'#f
                                    make-transformer-id #'#f
                                    #:tail-ids #'(tail-id)
                                    #:wrap-for-tail
                                    (lambda (body)
                                      (define-values (pattern idrs sidrs vars can-be-empty?)
                                        (convert-pattern #`(multi . #,gs-stx)))
                                      (with-syntax ([((p-id id-ref) ...) idrs]
                                                    [((s-id sid-ref) ...) sidrs])
                                        #`(syntax-parse tail-id
                                            [#,pattern
                                             (let ([p-id id-ref] ...)
                                               (let-syntax ([s-id sid-ref] ...)
                                                 #,body))])))))
