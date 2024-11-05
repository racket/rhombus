#lang racket/base
(require syntax/parse/pre
         (for-syntax racket/base
                     syntax/stx
                     syntax/parse/pre
                     "treelist.rkt"
                     "pack.rkt"
                     "entry-point-adjustment.rkt")
         (submod "quasiquote.rkt" convert)
         "quasiquote.rkt"
         (only-in "ellipsis.rkt"
                  [... rhombus...])
         (only-in "unquote-binding-primitive.rkt"
                  #%parens
                  &&
                  [:: uq::])
         (only-in "syntax-class-primitive.rkt"
                  Sequence)
         "dollar.rkt"
         "parse.rkt"
         "unquote-binding.rkt"
         "op-literal.rkt"
         "pack.rkt"
         "parens.rkt"
         (only-in "static-info.rkt"
                  define-static-info-syntax)
         (submod "syntax-object.rkt" for-quasiquote)
         "realm.rkt"
         "wrap-expression.rkt"
         "simple-pattern.rkt"
         "srcloc.rkt")

(provide (for-syntax parse-operator-definition-rhs
                     parse-operator-definitions-rhs
                     parse-transformer-definition-rhs
                     parse-transformer-definition-sequence-rhs))

(begin-for-syntax
  (struct parsed (fixity name opts-stx prec-stx assc-stx parsed-right? extra-kw-args
                         ;; implementation is function stx if `parsed-right?`,
                         ;; or a clause over #'self and maybe #'left otherwise
                         impl))
  (define (maybe-cons id ids) (if (syntax-e id) (cons id ids) ids)))
(define (make-all l) (pack-tail l))
(define (make-all-sequence l) (pack-multi-tail l))

(define (maybe-relocate-span in out)
  (if (or (not (syntax? out))
          (syntax-relocated-property out))
      out
      (syntax-relocated-property
       (relocate+reraw (unpack-tail in #f #f) out)
       #t)))

(define-unquote-binding-syntax _Term
  (unquote-binding-transformer
   (lambda (stx)
     (cond
       [(eq? 'term (current-unquote-binding-kind))
        (syntax-parse stx
          [(form-id . tail)
           (values #`(#,(syntax/loc #'form-id _) () () ())
                   #'tail)])]
       [else (values #'#f
                     #'())]))))

;; finish parsing one case (possibly the only case) in a macro definition,
;; now that we're in the right phase for the right-hand side of the definition
(define-for-syntax (parse-one-macro-definition pre-parsed adjustments case-shape)
  (define-values (who kind parsed-right?)
    (syntax-parse pre-parsed
      [(_ name _ _ kind _ _ _ _ parsed-right-id . _)
       (values #'name (syntax-e #'kind) (and (syntax-e #'parsed-right-id) #t))]))
  (define (macro-clause self-id all-id left-ids tail-pattern-in rhs)
    (define-values (tail-pattern implicit-tail?)
      (syntax-parse tail-pattern-in
        #:datum-literals (group)
        [(pat ... _::$-bind (tag::parens))
         ;; a "nothing" group pattern means no further tail
         #:when (free-identifier=? (in-unquote-binding-space (datum->syntax #'tag '#%parens))
                                   (unquote-bind-quote #%parens))
         (values #'(pat ...) #f)]
        [(pat ... _::$-bind _:identifier _::...-bind)
         ;; recognize where a tail match would be redundant and always be empty;
         ;; this is kind of an optimization, but one that's intended to be guaranteed;
         ;; note that this enables returning two values from the macro, instead
         ;; of just one
         (values tail-pattern-in #f)]
        [_ (values (if (or (syntax-e all-id)
                           (not parsed-right?))
                       #`((op $) (parens (group #%quotes (quotes (group #,@tail-pattern-in))
                                                && #%parens (parens (group all_tail uq:: Sequence)))))
                       tail-pattern-in)
                   #t)]))
    (define all*-id (if (syntax-e all-id)
                        all-id
                        (and implicit-tail? (not parsed-right?) #'all-for-relocate)))
    (define (add-maybe-relocate-span e)
      (if (and implicit-tail? (not parsed-right?))
          #`(maybe-relocate-span #,all*-id #,e)
          e))
    (syntax-parse tail-pattern
      [(dots::...-bind . _) (raise-syntax-error #f
                                                "misplaced repetition"
                                                #'dots)]
      [_ (void)])
    (define-values (pattern idrs sidrs vars can-be-empty?)
      (if implicit-tail?
          (convert-pattern #`(group (op $) _Term #,@tail-pattern (op $) tail (op rhombus...))
                           #:as-tail? #t
                           #:splice? #t
                           #:splice-pattern values)
          (convert-pattern #`(group (op $) _Term . #,tail-pattern)
                           #:as-tail? #t
                           #:splice? #t
                           #:splice-pattern values)))
    (with-syntax ([((id id-ref) ...) idrs]
                  [(((sid ...) sid-ref) ...) sidrs]
                  [(left-id ...) left-ids])
      (define body
        (cond
          [(eq? kind 'rule)
           (let ([ids (maybe-cons (or all*-id all-id)
                                  (cons self-id (append left-ids (syntax->list #'(id ... sid ... ...)))))])
             (if implicit-tail?
                 #`(values #,(add-maybe-relocate-span (convert-rule-template rhs ids))
                           (tail-rule-template tail))
                 (convert-rule-template rhs ids)))]
          [implicit-tail?
           #`(values #,(add-maybe-relocate-span #`(single-valued '#,who (lambda () (rhombus-body-expression #,rhs))))
                     (tail-rule-template tail))]
          [else
           #`(rhombus-body-expression #,rhs)]))
      #`[#,pattern
         (let ([id id-ref] ... [#,self-id self] [left-id left] ...)
           (define-static-info-syntax #,self-id #:getter get-syntax-static-infos)
           (define-static-info-syntax left-id #:getter get-syntax-static-infos)
           ...
           (let-syntaxes ([(sid ...) sid-ref] ...)
             #,@(if all*-id
                    #`((define #,all*-id
                         #,(if implicit-tail?
                               #`(make-all (list* left-id ... self (unpack-tail (rhombus-expression (group all_tail)) #f #f)))
                               #`(make-all (list* left-id ... self tail))))
                       (define-static-info-syntax #,all*-id #:getter get-syntax-static-infos))
                    '())
             #,body))]))
  (define (convert-rule-template block ids)
    (syntax-parse block
      #:datum-literals (group)
      [(_::block (group (_::quotes template)))
       ;; delay further conversion until after pattern variables are bound
       #`(rule-template template #,ids)]
      [(_::block (group e))
       (raise-syntax-error 'template "result must be a template expression" #'e)]))
  (syntax-parse pre-parsed
    #:datum-literals (pre-parsed infix prefix)
    ;; infix protocol
    [(pre-parsed name
                 _
                 infix
                 _
                 opt
                 (extra-kw-id ...)
                 prec
                 assc
                 parsed-right-id
                 [tail-pattern
                  self-id
                  all-id
                  left
                  (tag rhs ...)])
     (parsed 'infix
             #'name
             #'opt
             #'prec
             #'assc
             (and (syntax-e #'parsed-right-id) #t)
             (syntax->list #'(extra-kw-id ...))
             (cond
               [(syntax-e #'parsed-right-id)
                (define right-id #'parsed-right-id)
                (define extra-args (treelist->list (entry-point-adjustment-prefix-arguments adjustments)))
                #`(lambda (#,@extra-args left #,right-id self-id extra-kw-id ...)
                    (define-static-info-syntax left #:getter get-syntax-static-infos)
                    (define-static-info-syntax #,right-id #:getter get-syntax-static-infos)
                    (define-static-info-syntax self-id #:getter get-syntax-static-infos)
                    #,@(if (syntax-e #'all-id)
                           #`((define all-id (make-all (list left self-id #,right-id)))
                              (define-static-info-syntax all-id #:getter get-syntax-static-infos))
                           '())
                    #,(adjust-result
                       adjustments
                       2
                       (if (eq? kind 'rule)
                           (convert-rule-template #'(tag rhs ...)
                                                  (maybe-cons #'all-id (list #'left right-id #'self-id)))
                           #'(rhombus-body-expression (tag rhs ...)))))]
               [else
                (macro-clause #'self-id #'all-id (list #'left)
                              #'tail-pattern
                              #'(tag rhs ...))]))]
    ;; prefix protocol
    [(pre-parsed name
                 _
                 prefix
                 _
                 opt
                 (extra-kw-id ...)
                 prec
                 assc ; only non-#f if main (i.e., specified before `match` in the definition)
                 parsed-right-id
                 [tail-pattern
                  self-id
                  all-id
                  (tag rhs ...)])
     (parsed 'prefix
             #'name
             #'opt
             #'prec
             #'assc
             (and (syntax-e #'parsed-right-id) #t)
             (syntax->list #'(extra-kw-id ...))
             (cond
               [(syntax-e #'parsed-right-id)
                (define arg-id #'parsed-right-id)
                (define extra-args (treelist->list (entry-point-adjustment-prefix-arguments adjustments)))
                #`(lambda (#,@extra-args #,arg-id self-id extra-kw-id ...)
                    (define-static-info-syntax #,arg-id #:getter get-syntax-static-infos)
                    (define-static-info-syntax self-id #:getter get-syntax-static-infos)
                    #,@(if (syntax-e #'all-id)
                           #`((define all-id (make-all (list self-id #,arg-id)))
                              (define-static-info-syntax all-id #:getter get-syntax-static-infos))
                           '())
                    #,(adjust-result
                       adjustments
                       2
                       (if (eq? kind 'rule)
                           (convert-rule-template #'(tag rhs ...)
                                                  (maybe-cons #'all-id (list arg-id #'self-id)))
                           #`(rhombus-body-expression (tag rhs ...)))))]
               [else
                (cond
                  [(eq? case-shape 'cond)
                   ;; shortcut for a simple identifier macro; `self` and `tail` are bound
                   (define all*-id (if (syntax-e #'all-id)
                                       #'all-id
                                       (or (and (null? (syntax-e #'tail-pattern))
                                                #'all-for-relocate)
                                           #'#f)))
                   #`[#t
                      (let ([self-id self])
                        (define-static-info-syntax self-id #:getter get-syntax-static-infos)
                        #,@(maybe-bind-all all*-id #'self-id #'make-all #'tail-pattern #'tail)
                        #,@(maybe-bind-tail #'tail-pattern #'tail)
                        #,(maybe-return-tail
                           (let ([body (if (eq? kind 'rule)
                                           (convert-rule-template #'(tag rhs ...)
                                                                  (maybe-cons #'all-id (list #'self-id)))
                                           #`(rhombus-body-expression (tag rhs ...)))])
                             (if (and (syntax-e all*-id)
                                      (null? (syntax-e #'tail-pattern)))
                                 #`(maybe-relocate-span #,all*-id #,body)
                                 body))
                           #'tail-pattern
                           #'tail))]]
                  [else
                   (macro-clause #'self-id #'all-id '()
                                 #'tail-pattern
                                 #'(tag rhs ...))])]))]))

(define-for-syntax (select-case-shape pre-parsed)
  (syntax-parse pre-parsed
    #:datum-literals (prefix)
    [(pre-parsed _
                 _
                 prefix
                 _
                 _
                 _
                 _
                 _
                 _
                 [tail-pattern
                  . _])
     (if (is-simple-pattern? #'tail-pattern)
         ;; shortcut for identifier macros:
         'cond
         'syntax-parse)]
    [_ 'syntax-parse]))

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
                                                        #:datum-literals (group op)
                                                        [(_::parens (group (_::quotes (group (op _))))) #t]
                                                        [(_::quotes (group (op _))) #t]
                                                        [_ #f]))
                                            (raise-syntax-error 'template
                                                                (if (identifier? e)
                                                                    "expected an identifier that is bound by the pattern"
                                                                    "expected an identifier or a syntax object containing an operator")
                                                                e)))))]))

(define-syntax (tail-rule-template stx)
  (syntax-parse stx
    [(_ tail)
     (convert-template #'(multi (group (op $) tail (op rhombus...))))]))

;; combine previously parsed cases (possibly the only case) in a macro
;; definition that are all either prefix or infix
(define-for-syntax (build-cases ps prefix? make-id space-sym adjustments orig-stx case-shape)
  (unless (syntax-e make-id)
    (raise-syntax-error #f
                        (format "~a patterns are not allowed" (if prefix? "prefix" "infix"))
                        orig-stx))
  (define p (car ps))
  #`(#,make-id
     #,(parsed-prec-stx p)
     #,(if (parsed-parsed-right? p)
           #''automatic
           #''macro)
     (let ([#,(parsed-name p)
            #,(if (parsed-parsed-right? p)
                  (parsed-impl p)
                  (let ([extra-args (treelist->list (entry-point-adjustment-prefix-arguments adjustments))])
                    #`(lambda (#,@extra-args #,@(if prefix? '() (list #'left)) tail self #,@(parsed-extra-kw-args p))
                        #,(adjust-result
                           adjustments
                           2
                           (cond
                             [(eq? case-shape 'cond)
                              #`(cond
                                  #,@(map parsed-impl ps))]
                             [else
                              #`(syntax-parse (insert-multi-front-group self tail)
                                  #:disable-colon-notation
                                  #,@(map parsed-impl ps))])))))])
       #,(parsed-name p))
     #,@(if prefix?
            '()
            (list (parsed-assc-stx p)))))

;; single-case macro definition:
(define-for-syntax (parse-operator-definition-rhs orig-stx pre-parsed
                                                  space-sym
                                                  make-prefix-id make-infix-id
                                                  #:adjustments [adjustments no-adjustments]
                                                  #:extra-get-static-infoss [extra-get-static-infoss-stx #'()]
                                                  #:extra-shapes [extra-shapes '()])
  (define case-shape (select-case-shape pre-parsed))
  (define p (parse-one-macro-definition pre-parsed adjustments case-shape))
  (define op (parsed-name p))
  (define prefix? (eq? 'prefix (parsed-fixity p)))
  (define make-id (if prefix? make-prefix-id make-infix-id))
  (build-cases (list p) prefix? make-id space-sym adjustments orig-stx case-shape))

;; multi-case macro definition:
(define-for-syntax (parse-operator-definitions-rhs orig-stx pre-parseds
                                                   space-sym
                                                   make-prefix-id make-infix-id prefix+infix-id
                                                   #:adjustments [adjustments no-adjustments]
                                                   #:extra-get-static-infoss [extra-get-static-infoss-stx #'()]
                                                   #:extra-shapes [extra-shapes '()])
  (define case-shape 'syntax-parse)
  (define ps (map (lambda (p) (parse-one-macro-definition p adjustments case-shape)) pre-parseds))
  (define prefixes (for/list ([p (in-list ps)] #:when (eq? 'prefix (parsed-fixity p))) p))
  (define infixes (for/list ([p (in-list ps)] #:when (eq? 'infix (parsed-fixity p))) p))
  (define (check-fixity-consistent what options ps)
    (when (and (pair? ps) (pair? (cdr ps)))
      (for ([p (in-list ps)]
            [i (in-naturals)])
        (when (parsed-parsed-right? p)
          (raise-syntax-error #f
                              (format "multiple ~a cases not allowed;\n at least one pattern matches a parsed right-hand argument"
                                      what)
                              orig-stx))
        (unless (zero? i)
          (when (for*/or ([d (in-list (syntax->list (parsed-opts-stx p)))]
                          [d (in-list (or (syntax->list d) (list d)))])
                  (and (keyword? (syntax-e d))
                       (not (eq? '#:op_stx (syntax-e d)))
                       (not (eq? '#:all_stx (syntax-e d)))))
            (raise-syntax-error #f
                                (format "~a options not allowed after first ~a case"
                                        options what)
                                orig-stx))))))
  (check-fixity-consistent "prefix" "precedence" prefixes)
  (check-fixity-consistent "infix" "precedence and associativity" infixes)
  (when (null? infixes)
    (for ([p (in-list ps)])
      (when (keyword? (syntax-e (parsed-assc-stx p)))
        (raise-syntax-error #f
                            "associativity specified without infix cases"
                            orig-stx
                            (parsed-assc-stx p)))))
  (cond
    [(null? prefixes) (build-cases infixes #f make-infix-id space-sym adjustments orig-stx case-shape)]
    [(null? infixes) (build-cases prefixes #t make-prefix-id space-sym adjustments orig-stx case-shape)]
    [else #`(#,prefix+infix-id
             #,(build-cases prefixes #t make-prefix-id space-sym adjustments orig-stx case-shape)
             #,(build-cases infixes #f make-infix-id space-sym adjustments orig-stx case-shape))]))

(define-for-syntax (adjust-result adjustments arity b)
  (wrap-expression ((entry-point-adjustment-wrap-body adjustments) arity #`(parsed #:rhombus/expr #,b))))

;; ----------------------------------------

(define-for-syntax (parse-transformer-definition-rhs pre-parseds self-ids all-ids extra-bindss
                                                     make-transformer-id
                                                     extra-get-static-infoss-stx
                                                     extra-shapes
                                                     #:tail-ids [tail-ids '()]
                                                     #:wrap-for-tail [wrap-for-tail values]
                                                     #:else [else-case #f]
                                                     #:cut? [cut? #f]
                                                     #:report-keywords [report-keywords #f])
  (define case-shape (select-transformer-case-shape pre-parseds extra-bindss))
  (define in-extra-ids (generate-temporaries (car extra-bindss)))
  (with-syntax ([((_ id . _) . _) pre-parseds])
    #`(#,make-transformer-id
       (let ([id (lambda (tail #,@tail-ids self #,@in-extra-ids)
                   #,(cond
                       [(eq? case-shape 'cond)
                        ;; shortcut for simple patterns
                        #`(cond
                            #,@(for/list ([pre-parsed (in-list pre-parseds)]
                                          [self-id (in-list self-ids)]
                                          [all-id (in-list all-ids)])
                                 (syntax-parse pre-parsed
                                   #:datum-literals (pre-parsed)
                                   [(pre-parsed id
                                                _
                                                tail-pattern
                                                rhs)
                                    #`[#t
                                       (let ([#,self-id self])
                                         #,(generate-simple-pattern-check self-id #'tail-pattern #'tail)
                                         (define-static-info-syntax #,self-id #:getter get-syntax-static-infos)
                                         #,@(maybe-bind-all all-id self-id #'make-all #'tail-pattern #'tail)
                                         #,@(maybe-bind-tail #'tail-pattern #'tail)
                                         #,(wrap-for-tail
                                            #`(rhombus-body-expression rhs)))]]))
                            #,@(if else-case
                                   #`([else #,else-case])
                                   null))]
                       [else
                        ;; general pattern mode
                        #`(syntax-parse (insert-multi-front-group self tail)
                            #:disable-colon-notation
                            #,@(for/list ([pre-parsed (in-list pre-parseds)]
                                          [self-id (in-list self-ids)]
                                          [all-id (in-list all-ids)]
                                          [extra-binds-stx (in-list extra-bindss)])
                                 (syntax-parse pre-parsed
                                   #:datum-literals (pre-parsed)
                                   [(pre-parsed id
                                                _
                                                tail-pattern
                                                rhs)
                                    (define-values (pattern idrs sidrs vars can-be-empty?) (convert-pattern #`(group (op $) _Term . tail-pattern)
                                                                                                            #:as-tail? #t
                                                                                                            #:splice? #t
                                                                                                            #:splice-pattern values))
                                    (define-values (extra-patterns wrap-extra)
                                      (build-extra-patterns in-extra-ids extra-binds-stx extra-get-static-infoss-stx extra-shapes))
                                    (with-syntax ([((p-id id-ref) ...) idrs]
                                                  [(((s-id ...) sid-ref) ...) sidrs])
                                      #`[#,pattern
                                         #,@(if cut? #'(#:cut) '())
                                         #,@extra-patterns
                                         #,@(build-extra-bindings in-extra-ids extra-binds-stx extra-get-static-infoss-stx extra-shapes)
                                         (define #,self-id self)
                                         (define-static-info-syntax #,self-id #:getter get-syntax-static-infos)
                                         #,@(if (syntax-e all-id)
                                                #`((define #,all-id (make-all (cons self (unpack-tail tail #f #f))))
                                                   (define-static-info-syntax #,all-id #:getter get-syntax-static-infos))
                                                '())
                                         #,(wrap-extra
                                            #`(let ([p-id id-ref] ...)
                                                (let-syntaxes ([(s-id ...) sid-ref] ...)
                                                  #,(wrap-for-tail
                                                     #`(rhombus-body-expression rhs)))))])]))
                            #,@(if else-case
                                   #`([_ #,else-case])
                                   null))]))])
         id)
       #,@(if report-keywords
              #`((quote #,(hash-keys
                           (for/hasheq ([extra-binds-stx (in-list extra-bindss)]
                                        #:when #t
                                        [extra-bind (in-list (syntax->list extra-binds-stx))]
                                        [kw (in-list report-keywords)]
                                        #:when (syntax-e extra-bind))
                             (values kw #t))
                           #t)))
              null))))

(define-for-syntax (parse-transformer-definition-sequence-rhs pre-parsed self-id all-id
                                                              make-transformer-id
                                                              gs-stx)
  (parse-transformer-definition-rhs (list pre-parsed) (list self-id) (list #'#f) (list #'())
                                    make-transformer-id #'() (list)
                                    #:tail-ids (list #'orig-head #'extra-tail)
                                    #:wrap-for-tail
                                    (lambda (body)
                                      (define-values (pattern idrs sidrs vars can-be-empty?)
                                        (convert-pattern #`(multi . #,gs-stx)))
                                      (with-syntax ([((p-id id-ref) ...) idrs]
                                                    [(((s-id ...) sid-ref) ...) sidrs])
                                        #`(syntax-parse extra-tail
                                            #:context (insert-multi-front-head-group orig-head extra-tail)
                                            #:disable-colon-notation
                                            [#,pattern
                                             #,@(if (syntax-e all-id)
                                                    #`((define #,all-id (make-all-sequence (cons orig-head
                                                                                                 (unpack-multi extra-tail #f #f))))
                                                       (define-static-info-syntax #,all-id #:getter get-syntax-static-infos))
                                                    '())
                                             (let ([p-id id-ref] ...)
                                               (let-syntaxes ([(s-id ...) sid-ref] ...)
                                                 #,body))])))))

(define-for-syntax (select-transformer-case-shape pre-parseds extra-bindss)
  (cond
    [(and (pair? pre-parseds) (null? (cdr pre-parseds))
          (syntax-parse (car pre-parseds)
            #:datum-literals (pre-parsed)
            [(pre-parsed _
                         _
                         tail-pattern
                         _)
             (is-simple-pattern? #'tail-pattern)]
            [_ #f])
          (andmap stx-null? extra-bindss))
     'cond]
    [else 'syntax-parse]))

(define (single-valued who thunk)
  (call-with-values
   thunk
   (case-lambda
     [(v) v]
     [args (apply raise-result-arity-error* who rhombus-realm 1 #f args)])))


(define-for-syntax (build-extra-patterns in-extra-ids extra-binds-stx extra-get-static-infoss-stx extra-shapes)
  (for/fold ([rev-withs '()]
             [wrap (lambda (x) x)]
             #:result (values (reverse rev-withs)
                              wrap))
            ([in-extra-id (in-list in-extra-ids)]
             [extra-bind (in-list (syntax->list extra-binds-stx))]
             [extra-get-static-infos (in-list (syntax->list extra-get-static-infoss-stx))]
             [extra-shape (in-list extra-shapes)]
             #:when (syntax-e extra-bind)
             #:when (eq? extra-shape 'pattern))
    (define-values (pattern idrs sidrs vars can-be-empty?)
      (syntax-parse extra-bind
        [(_ g)
         (convert-pattern #'g
                          #:as-tail? #t
                          #:splice? #t
                          #:splice-pattern values)]))
    (with-syntax ([((p-id id-ref) ...) idrs]
                  [(((s-id ...) sid-ref) ...) sidrs])
      (values
       (append (reverse (syntax->list
                         #`(#:with #,pattern (unpack-tail #,in-extra-id #f #f))))
               rev-withs)
       (lambda (x)
         #`(let ([p-id id-ref] ...)
             (let-syntaxes ([(s-id ...) sid-ref] ...)
               #,(wrap x))))))))

(define-for-syntax (build-extra-bindings in-extra-ids extra-binds-stx extra-get-static-infoss-stx extra-shapes)
  (apply
   append
   (for/list ([in-extra-id (in-list in-extra-ids)]
              [extra-bind (in-list (syntax->list extra-binds-stx))]
              [extra-get-static-infos (in-list (syntax->list extra-get-static-infoss-stx))]
              [extra-shape (in-list extra-shapes)]
              #:when (syntax-e extra-bind)
              #:unless (eq? extra-shape 'pattern))
     (list
      #`(define #,extra-bind #,in-extra-id)
      #`(define-static-info-syntax #,extra-bind #:getter #,extra-get-static-infos)))))
