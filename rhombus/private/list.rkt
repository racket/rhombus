#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx
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
         "function-arity-key.rkt"
         "sequence-constructor-key.rkt"
         "op-literal.rkt"
         "literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         "repetition.rkt"
         "compound-repetition.rkt"
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parse.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "define-arity.rkt"
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

(define-for-syntax list-static-infos
  #'((#%index-get list-ref)
     (#%append List.append)
     (#%sequence-constructor in-list)
     (#%dot-provider list-instance)))

(define-binding-syntax List.cons
  (binding-transformer
   (let ([composite (make-composite-binding-transformer
                     "List.cons" #'nonempty-list? (list #'car) (list #'())
                     #:index-result-info? #t
                     #:rest-accessor #'cdr
                     #:rest-repetition? #f)])
     (lambda (tail)
       (syntax-parse tail
         #:datum-literals (parens)
         [(form-id ((~and tag parens) elem list) . new-tail)
          (composite #'(form-id (tag elem) . new-tail)
                     #`(#,group-tag rest-bind #,list-static-infos
                        #:annot-prefix? #f
                        list))])))))

(define/arity (List.cons a d)
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (list? d) (raise-argument-error* 'List.cons rhombus-realm "List" d))
  (cons a d))

(define/arity (first l)
  (unless (list? l) (raise-argument-error* 'List.first rhombus-realm "List" l))
  (car l))

(define/arity (rest l)
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (and (pair? l) (list? l)) (raise-argument-error* 'List.rest rhombus-realm "NonemptyList" l))
  (cdr l))

(define iota
  (lambda (n)
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error* 'List.iota rhombus-realm "NonnegInt" n))
    (for/list ([i (in-range n)])
      i)))

(define-static-info-syntax length
  (#%function-arity 2))

(define-static-info-syntax reverse
  (#%function-arity 2))

(define-name-root List
  #:fields
  (length
   [cons List.cons]
   first
   rest
   [empty null]
   reverse
   iota
   [map List.map]
   [for_each List.for_each]
   [sort List.sort]
   [drop_left List.drop_left]
   [drop_right List.drop_right]
   [has_element List.has_element]
   [remove List.remove]
   repet
   of
   [append List.append]))

(define-syntax List
  (expression-transformer
   ;; special cases optimize for `...` and `&`; letting it expand
   ;; instead to `(apply list ....)` is not so bad, but but we can
   ;; avoid a `list?` check in `apply`, and we can expose more static
   ;; information this way
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (parens group op)
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
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
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

(define-binding-syntax null
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-infoer #'()) #'tail)]))))

(define-syntax (empty-infoer stx)
  (syntax-parse stx
    [(_ static-infos datum)
     (binding-info "List.empty"
                   #'empty
                   #'static-infos
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

(define-name-root NonemptyList
  #:fields
  ([of NonemptyList.of]))

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

(define-syntax list-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'length)]
        [(first) (field (lambda (e reloc)
                          (wrap-static-info* (reloc #`(car #,e))
                                             (or (syntax-local-static-info e #'#%index-result)
                                                 #'()))))]
        [(rest) (field (lambda (e reloc)
                         (wrap-static-info* (reloc #`(cdr #,e))
                                            (datum->syntax #f
                                                           (or (extract-static-infos e)
                                                               '())))))]
        [(reverse) (0ary #'reverse list-static-infos)]
        [(map) (nary #'List.map 2 #'List.map list-static-infos)]
        [(for_each) (nary #'List.for_each 2 #'List.for_each)]
        [(sort) (nary #'List.sort 3 #'List.sort list-static-infos)]
        [(drop_left) (nary #'List.drop_left 2 #'List.drop_left list-static-infos)]
        [(drop_right) (nary #'List.drop_right 2 #'List.drop_right list-static-infos)]
        [(has_element) (nary #'List.has_element 2 #'List.has_element #'())]
        [(remove) (nary #'List.remove 2 #'List.remove list-static-infos)]
        [else (fail-k)])))))

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
       #:datum-literals (parens group repet)
       [(form-id (~and args (parens g)) . tail)
        (define name (syntax-e #'form-id))
        (values (make-repetition-info #'(form-id args)
                                      name
                                      #`(check-repetition-list '#,name (rhombus-expression g))
                                      1
                                      0
                                      #'()
                                      #f)
                #'tail)]))))

(define (check-repetition-list who v)
  (unless (list? v)
    (raise-argument-error who "List" v))
  v)

(define-static-info-syntax list
  (#%call-result #,list-static-infos)
  (#%function-arity -1))

(define-static-info-syntax iota
  (#%call-result #,list-static-infos)
  (#%function-arity 2))

(define/arity (List.map lst proc)
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (procedure? proc)
    (raise-argument-error* 'List.map rhombus-realm "Function" proc))
  (map proc lst))

(define/arity (List.for_each lst proc)
  (unless (procedure? proc)
    (raise-argument-error* 'List.for_each rhombus-realm "Function" proc))
  (for-each proc lst))

(define/arity (List.sort lst [less-than? <])
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (and (procedure? less-than?)
               (procedure-arity-includes? less-than? 2))
    (raise-argument-error* 'List.sort rhombus-realm "Function.of_arity(2)" less-than?))
  (sort lst less-than?))

(define/arity List.append
  #:static-infos ((#%call-result #,list-static-infos))
  (case-lambda
    [() null]
    [(a)
     (unless (list? a) (raise-argument-error* 'List.append rhombus-realm "List" a))
     a]
    [(a b)
     (cond
       [(list? b) (append a b)]
       [else
        (unless (list? a) (raise-argument-error* 'List.append rhombus-realm "List" a))
        (raise-argument-error* 'List.append rhombus-realm "List" b)])]
    [ls
     (if (list? (let loop ([ls ls]) (if (pair? (cdr ls)) (loop (cdr ls)) (car ls))))
         (apply append ls)
         (for ([l (in-list ls)])
           (unless (list? l) (raise-argument-error* 'List.append rhombus-realm "List" l))))]))

(define/arity (List.drop_left l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (list-tail l n))

(define/arity (List.drop_right l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (unless (list? l) (raise-argument-error* 'drop_right rhombus-realm "List" l))
  (unless (exact-nonnegative-integer? n) (raise-argument-error* 'drop_right rhombus-realm "NonnegInt" n))
  (define len (length l))
  (when (n . >= . len)
    (raise-arguments-error* 'drop_right rhombus-realm
                            "list is shorter than the number of elements to drop"
                            "list length" len
                            "number to drop" n))
  (for/list ([a (in-list l)]
             [i (in-range 0 (- len n))])
    a))

(define/arity (List.remove l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (remove n l equal-always?))

(define/arity (List.has_element l n)
  (and (member n l equal-always?) #t))

(define list-method-table
  (hash 'length (method1 length)
        'first car
        'rest cdr
        'reverse (method1 reverse)
        'drop_left (method2 List.drop_left)
        'drop_right (method2 List.drop_right)
        'has_element (method2 List.has_element)
        'remove (method2 List.remove)
        'append (method* List.append)
        'map (method1 List.map)
        'for_each (method1 List.for_each)))

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
                           (length-at-least v #,len))))
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
                           (length-at-least v #,len))))
     (generate-binding #'form-id pred args #'tail #'rest-arg
                       (if (null? args) #'values #'cdr)
                       #t)]
    [(form-id (_ arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail)]))

(define-for-syntax (parse-list-form stx
                                    #:repetition? repetition?
                                    #:span-form-name? span-form-name?)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (~and args (tag arg ...)) . tail)
     ;; a list of syntax, (cons 'splice syntax), and (cons 'rep syntax):
     (define content
       (let loop ([gs-stx #'(arg ...)] [accum '()])
         (syntax-parse gs-stx
           #:datum-literals (group op)
           [() (reverse accum)]
           [((group _::&-expr rand ...+) . gs)
            (define e (let ([g #'(group rand ...)])
                        (if repetition?
                            (syntax-parse g
                              [rep::repetition #'rep.parsed])
                            (syntax-parse g
                              [e::expression #'e.parsed]))))
            (loop #'gs (cons (list 'splice e) accum))]
           [(rep-arg (group _::...-expr) . gs)
            (define-values (new-gs extras) (consume-extra-ellipses #'gs))
            (define e (syntax-parse #'rep-arg
                        [rep::repetition
                         (define the-rep (flatten-repetition #'rep.parsed extras))
                         (if repetition?
                             the-rep
                             (repetition-as-list the-rep 1))]))
            (loop new-gs (cons (list 'rep e) accum))]
           [(g . gs)
            (define e (if repetition?
                          (syntax-parse #'g
                            [rep::repetition #'rep.parsed])
                          (syntax-parse #'g
                            [e::expression #'e.parsed])))
            (loop #'gs (cons e accum))])))
     (define src-span (if span-form-name?
                          (respan (datum->syntax #f (list #'form-id #'args)))
                          (maybe-respan #'args)))
     (define (tag-props stx) (datum->syntax stx (syntax-e stx) stx #'tag))
     (values
      (relocate-wrapped
       src-span
       (cond
         [(and (pair? content) (null? (cdr content))
               (pair? (car content)) (eq? 'rep (caar content)))
          ;; special case, especially to expose static info on rest elements
          (define seq (cadar content))
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
           #:is-sequence? (lambda (e) (and (pair? e) (eq? 'rep (car e))))
           #:extract (lambda (e) (if (pair? e) (cadr e) e))
           (lambda new-content
             (let ([content (for/list ([e (in-list content)]
                                       [new-e (in-list new-content)])
                              (if (pair? e) (list (car e) new-e) new-e))])
               (values (tag-props (build-list-form content))
                       list-static-infos))))]))
      #'tail)]))

(define-for-syntax (build-list-form content)
  ;; group content into a list of list-generating groups,
  ;; and those lists will be appended
  (let loop ([content content] [accum '()] [accums '()])
    (define (gather)
      (if (null? accum) accums (cons #`(list #,@(reverse accum)) accums)))
    (cond
      [(null? content)
       (define es (gather))
       (cond
         [(null? es) #'null]
         [(null? (cdr es)) (car es)]
         [else #`(append #,@(reverse es))])]
      [(pair? (car content))
       (cond
         [(eq? (caar content) 'splice)
          (loop (cdr content) '() (cons #`(assert-list #,(cadar content)) (gather)))]
         [else
          (loop (cdr content) '() (cons (cadar content) (gather)))])]
      [else
       (loop (cdr content) (cons (car content) accum) accums)])))

(define-for-syntax (parse-list-expression stx)
  (parse-list-form stx #:repetition? #f #:span-form-name? #f))

(define-for-syntax (parse-list-repetition stx)
  (parse-list-form stx #:repetition? #t #:span-form-name? #f))

(define (length-at-least v len)
  (or (eqv? len 0)
      (and (pair? v)
           (length-at-least (cdr v) (- len 1)))))

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
