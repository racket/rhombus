#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         "provide.rkt"
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "reducer.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "call-result-key.rkt"
         "index-result-key.rkt"
         "sequence-constructor-key.rkt"
         "op-literal.rkt"
         "literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         "repetition.rkt"
         "compound-repetition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "define-arity.rkt"
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         "rest-bind.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer)
                     List)
         (for-spaces (rhombus/namespace
                      rhombus/annot)
                     NonemptyList))

(module+ for-binding
  (provide (for-syntax parse-list-binding
                       parse-list-expression
                       parse-list-repetition)))

(module+ for-builtin
  (provide list-method-table))

(module+ for-implicit
  (provide (for-syntax set-#%call-ids!)))

(module+ for-compound-repetition
  (provide (for-syntax list-static-infos)))

(module+ normal-call
  (provide (for-syntax normal-call?)))

(define-primitive-class List list
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get List.get)
                          (#%append List.append)
                          (#%sequence-constructor in-list))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons List.cons]
   [empty null]
   [iota List.iota]
   repet
   of
   )
  #:properties
  ([first List.first
          (lambda (e)
            (syntax-local-static-info e #'#%index-result))]
   [rest List.rest
         (lambda (e)
           (define maybe-index-result
             (syntax-local-static-info e #'#%index-result))
           (if maybe-index-result
               #`((#%index-result #,maybe-index-result)
                  . #,list-static-infos)
               list-static-infos))]
   )
  #:methods
  (length
   reverse
   append
   drop_left
   drop_right
   has_element
   remove
   map
   for_each
   sort
   ))

(define-name-root NonemptyList
  #:fields
  ([of NonemptyList.of]))

(define-binding-syntax List.cons
  (binding-transformer
   (let ([composite (make-composite-binding-transformer
                     "List.cons" #'nonempty-list? (list #'car) (list #'())
                     #:static-infos list-static-infos
                     #:index-result-info? #t
                     #:rest-accessor #'cdr
                     #:rest-repetition? #f)])
     (lambda (tail)
       (syntax-parse tail
         [(form-id (tag::parens elem list) . new-tail)
          (composite #'(form-id (tag elem) . new-tail)
                     #`(#,group-tag rest-bind #,list-static-infos
                        #:annot-prefix? #f
                        list))])))))

(set-primitive-contract! 'list? "List")

(define (check-list who l)
  (unless (list? l)
    (raise-argument-error* who rhombus-realm "List" l)))

(define/arity (List.cons a d)
  #:inline
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who d)
  (cons a d))

(define (check-nonempty-list who l)
  (unless (and (pair? l) (list? l))
    (raise-argument-error* who rhombus-realm "NonemptyList" l)))

(define/arity (List.first l)
  #:inline
  (check-nonempty-list who l)
  (car l))

(define/arity (List.rest l)
  #:inline
  #:static-infos ((#%call-result #,list-static-infos))
  (check-nonempty-list who l)
  (cdr l))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error* who rhombus-realm "NonnegInt" n)))

(define/arity (List.iota n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-nonneg-int who n)
  (for/list ([i (in-range n)])
    i))

(define/method (List.length l)
  #:inline
  #:primitive (length)
  (length l))

(define/method (List.reverse l)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (reverse l))

(define-syntax List
  (expression-transformer
   ;; special cases optimize for `...` and `&`; letting it expand
   ;; instead to `(apply list ....)` is not so bad, but but we can
   ;; avoid a `list?` check in `apply`, and we can expose more static
   ;; information this way
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (tag::parens _ ... _ (group _::...-expr)) . tail)
        #:when (normal-call? #'tag)
        (parse-list-form stx #:repetition? #f #:span-form-name? #t)]
       [(form-id (tag::parens _ ... (group _::&-expr _ ...)) . tail)
        #:when (normal-call? #'tag)
        (parse-list-form stx #:repetition? #f #:span-form-name? #t)]
       [(_ . tail)
        (values #'list #'tail)]))))

(define-binding-syntax List
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (_::parens arg ...) . tail)
        (parse-list-binding stx)]))))

(define-annotation-constructor (List of)
  ()
  #'list? list-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-list #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'list-build-convert #'())

(define-syntax (list-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst '()] #:result (and lst (reverse lst)))
              ([v (in-list #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (cons v lst))
       (lambda () #f))))

(define-static-info-syntax null
  #:defined list-static-infos)

(define-binding-syntax null
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-infoer #'()) #'tail)]))))

(define-syntax (empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "List.empty"
                   #'empty
                   (static-infos-union list-static-infos #'up-static-infos)
                   #'()
                   #'empty-matcher
                   #'literal-bind-nothing
                   #'literal-commit-nothing
                   #'datum)]))

(define-syntax (empty-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (null? arg-id)
           success
           fail)]))

(define (nonempty-list? l)
  (and (pair? l) (list? l)))

(define-annotation-constructor (NonemptyList NonemptyList.of)
  ()
  #'nonempty-list? list-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-list #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'nonempty-list-build-convert #'())

(define-syntax (nonempty-list-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst '()] #:result (and lst (reverse lst)))
              ([v (in-list #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (cons v lst))
       (lambda () #f))))

(define-reducer-syntax List
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer/no-break #'build-reverse
                                  #'([accum null])
                                  #'build-accum
                                  list-static-infos
                                  #'accum)
                #'tail)]))))

(define-syntax (build-reverse stx)
  (syntax-parse stx
    [(_ accum e) #'(reverse e)]))

(define-syntax (build-accum stx)
  (syntax-parse stx
    [(_ accum e) #'(cons e accum)]))

(define-repetition-syntax repet
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and args (_::parens g)) . tail)
        (values (make-repetition-info #'(form-id args)
                                      #'repet
                                      #'(let ([l (rhombus-expression g)])
                                          (check-list 'List.repet l)
                                          l)
                                      1
                                      0
                                      #'()
                                      #f)
                #'tail)]))))

(define (check-function-of-arity n who proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc n))
    (raise-argument-error* who rhombus-realm
                           (string-append "Function.of_arity("
                                          (number->string n)
                                          ")")
                           proc)))

(define/method (List.map lst proc)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (map proc lst))

(define/method (List.for_each lst proc)
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (for-each proc lst))

(define/method (List.sort lst [less-than? <])
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who lst)
  (check-function-of-arity 2 who less-than?)
  (sort lst less-than?))

;; only check that the *last* argument is list here
(define/method List.append
  #:inline
  #:primitive (append)
  #:static-infos ((#%call-result #,list-static-infos))
  (case-lambda
    [() null]
    [(a)
     (check-list who a)
     a]
    [(a b)
     (unless (list? b)
       (check-list who a)
       (raise-argument-error* who rhombus-realm "List" b))
     (append a b)]
    [ls
     (let ([ln (let loop ([ls ls])
                 (if (pair? (cdr ls)) (loop (cdr ls)) (car ls)))])
       (unless (list? ln)
         (for ([l (in-list ls)])
           (check-list who l))
         (raise-argument-error* who rhombus-realm "List" ln)))
     (apply append ls)]))

;; these primitives don't check for listness
(define/arity (List.get l n)
  #:inline
  #:primitive (list-ref)
  (check-list who l)
  (list-ref l n))

(define/method (List.drop_left l n)
  #:inline
  #:primitive (list-tail)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (list-tail l n))

(define/method (List.drop_right l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (check-nonneg-int who n)
  (define len (length l))
  (when (n . > . len)
    (raise-arguments-error* who rhombus-realm
                            "list is shorter than the number of elements to drop"
                            "list length" len
                            "number to drop" n))
  (for/list ([a (in-list l)]
             [i (in-range 0 (- len n))])
    a))

(define/method (List.remove l v)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (remove v l equal-always?))

(define/method (List.has_element l v)
  (check-list who l)
  (and (member v l equal-always?) #t))

(define-for-syntax (wrap-list-static-info expr)
  (wrap-static-info* expr list-static-infos))

;; parses a list pattern that has already been checked for use with a
;; suitable `parens` or `brackets` form
(define-for-syntax (parse-list-binding stx)
  (define (generate-binding form-id pred args tail [rest-arg #f] [rest-selector #f]
                            [rest-repetition? #t])
    ((make-composite-binding-transformer "List"
                                         pred
                                         #:steppers (if (null? args)
                                                        null
                                                        (cons #'values
                                                              (for/list ([arg (in-list (cdr args))])
                                                                #'cdr)))
                                         (for/list ([arg (in-list args)])
                                           #'car)
                                         (for/list ([arg (in-list args)])
                                           #'())
                                         #:index-result-info? #t
                                         #:rest-accessor rest-selector
                                         #:rest-repetition? rest-repetition?
                                         #:static-infos list-static-infos)
     #`(#,form-id (parens . #,args) . #,tail)
     rest-arg))
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (_ arg ... (group _::&-bind rest-arg ...)) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (maybe-list-tail v '#,len)
                           #t)))
     (generate-binding #'form-id pred args #'tail
                       #`(#,group-tag rest-bind #,list-static-infos
                          (#,group-tag rest-arg ...))
                       (if (null? args) #'values #'cdr)
                       #f)]
    [(form-id (_ arg ... rest-arg (group _::...-bind)) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (maybe-list-tail v '#,len)
                           #t)))
     (generate-binding #'form-id pred args #'tail #'rest-arg
                       (if (null? args) #'values #'cdr)
                       #t)]
    [(form-id (_ arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (null? (maybe-list-tail v '#,len)))))
     (generate-binding #'form-id pred args #'tail)]))

(begin-for-syntax
  (struct list-rest (syntax))
  (struct list-rest-splice list-rest ())
  (struct list-rest-rep list-rest ()))

(define-for-syntax (parse-list-form stx
                                    #:repetition? repetition?
                                    #:span-form-name? span-form-name?)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (~and args (tag arg ...)) . tail)
     ;; a list of syntax, (list-rest-splice syntax), or (list-rest-rep syntax):
     (define content
       (let loop ([gs-stx #'(arg ...)])
         (syntax-parse gs-stx
           #:datum-literals (group)
           [() '()]
           [(rep-arg (group _::...-expr) . gs)
            (define-values (new-gs extras) (consume-extra-ellipses #'gs))
            (define e (syntax-parse #'rep-arg
                        [rep::repetition
                         (define the-rep (flatten-repetition #'rep.parsed extras))
                         (if repetition?
                             the-rep
                             (repetition-as-list the-rep 1))]))
            (cons (list-rest-rep e)
                  (loop new-gs))]
           [((~or* (~and (group _::&-expr rand ...+)
                         (~parse g #`(#,group-tag rand ...))
                         (~bind [splice? #t]))
                   g)
             . gs)
            (define e (if repetition?
                          (syntax-parse #'g
                            [rep::repetition #'rep.parsed])
                          (syntax-parse #'g
                            [e::expression #'e.parsed])))
            (cons (if (attribute splice?)
                      (list-rest-splice e)
                      e)
                  (loop #'gs))])))
     (define src-span (if span-form-name?
                          (respan (datum->syntax #f (list #'form-id #'args)))
                          (maybe-respan #'args)))
     (define (tag-props stx) (datum->syntax stx (syntax-e stx) stx #'tag))
     (values
      (relocate-wrapped
       src-span
       (cond
         [(and (pair? content) (null? (cdr content))
               (list-rest-rep? (car content)))
          ;; special case, especially to expose static info on rest elements
          (define seq (list-rest-syntax (car content)))
          (cond
            [repetition? (repetition-as-deeper-repetition seq list-static-infos)]
            [else (wrap-list-static-info seq)])]
         [(not repetition?)
          (wrap-list-static-info
           (tag-props
            (build-list-form content)))]
         [else
          (build-compound-repetition
           stx
           content
           #:is-sequence? list-rest-rep?
           #:extract (lambda (e) (if (list-rest? e) (list-rest-syntax e) e))
           (lambda new-content
             (let ([content (for/list ([e (in-list content)]
                                       [new-e (in-list new-content)])
                              (cond
                                [(list-rest-splice? e) (list-rest-splice new-e)]
                                [(list-rest-rep? e) (list-rest-rep new-e)]
                                [else new-e]))])
               (values (tag-props (build-list-form content))
                       list-static-infos))))]))
      #'tail)]))

(define-for-syntax (build-list-form content)
  ;; group content into a list of list-generating groups,
  ;; and those lists will be appended
  (define (gather group groups)
    (if (null? group)
        groups
        (cons #`(list #,@group) groups)))
  (define groups
    (for/foldr ([group '()]
                [groups '()]
                #:result (gather group groups))
               ([one (in-list content)])
      (cond
        [(list-rest-splice? one)
         (values '()
                 (cons #`(assert-list #,(list-rest-syntax one))
                       (gather group groups)))]
        [(list-rest-rep? one)
         (values '()
                 (cons (list-rest-syntax one)
                       (gather group groups)))]
        [else (values (cons one group) groups)])))
  (cond
    [(null? groups) #'null]
    [(null? (cdr groups)) (car groups)]
    [else #`(append #,@groups)]))

(define-for-syntax (parse-list-expression stx)
  (parse-list-form stx #:repetition? #f #:span-form-name? #f))

(define-for-syntax (parse-list-repetition stx)
  (parse-list-form stx #:repetition? #t #:span-form-name? #f))

(define (maybe-list-tail l n)
  (or (and (eqv? n 0)
           l)
      (and (pair? l)
           (maybe-list-tail (cdr l) (sub1 n)))))

(define (assert-list v)
  (unless (list? v)
    (raise-arguments-error* 'List rhombus-realm
                            "not a list for splicing"
                            "value" v))
  v)

(begin-for-syntax
  (define #%call-id #f)
  (define static-#%call-id #f)
  (define (set-#%call-ids! id static-id)
    (set! #%call-id id)
    (set! static-#%call-id static-id))
  (define (normal-call? tag)
    (define id (datum->syntax tag '#%call))
    (or (free-identifier=? #%call-id id)
        (free-identifier=? static-#%call-id id))))
