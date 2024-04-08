#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "tag.rkt")
         racket/vector
         "treelist.rkt"
         (submod "treelist.rkt" unsafe)
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
         "rest-bind.rkt"
         "number.rkt")

(provide (for-spaces (rhombus/namespace
                      #f
                      rhombus/repet
                      rhombus/bind
                      rhombus/annot
                      rhombus/reducer)
                     List
                     PairList)
         (for-spaces (rhombus/namespace
                      rhombus/annot)
                     NonemptyList
                     NonemptyPairList))

(module+ for-binding
  (provide (for-syntax parse-list-binding
                       parse-list-expression
                       parse-list-repetition)))

(module+ for-builtin
  (provide treelist-method-table
           list-method-table))

(module+ for-implicit
  (provide (for-syntax set-#%call-ids!)))

(module+ for-compound-repetition
  (provide (for-syntax list-static-infos
                       treelist-static-infos)))

(module+ for-listable
  (provide prop:Listable Listable? Listable-ref
           treelist? listable?
           to-treelist to-list
           (for-syntax treelist-static-infos)))

(module+ normal-call
  (provide (for-syntax normal-call?)))

(define-primitive-class List treelist
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get List.get)
                          (#%append List.append)
                          (#%sequence-constructor in-treelist))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons List.cons]
   [empty empty-treelist]
   [iota List.iota]
   [repet List.repet]
   [of List.of])
  #:properties
  ([first List.first
          (lambda (e)
            (syntax-local-static-info e #'#%index-result))]
   [last List.last
         (lambda (e)
           (syntax-local-static-info e #'#%index-result))]
   [rest List.rest
         (lambda (e)
           (define maybe-index-result
             (syntax-local-static-info e #'#%index-result))
           (if maybe-index-result
               #`((#%index-result #,maybe-index-result)
                  . #,treelist-static-infos)
               treelist-static-infos))])
  #:methods
  (length
   get
   set
   add
   insert
   delete
   reverse
   append
   take
   take_last
   drop
   drop_last
   sublist
   has_element
   find
   remove
   map
   for_each
   sort
   to_list))

(define-primitive-class PairList list
  #:lift-declaration
  #:constructor-arity -1
  #:instance-static-info ((#%index-get PairList.get)
                          (#%append PairList.append)
                          (#%sequence-constructor in-list))
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  ([cons PairList.cons]
   [empty null]
   [iota PairList.iota]
   [repet PairList.repet]
   [of PairList.of])
  #:properties
  ([first PairList.first
          (lambda (e)
            (syntax-local-static-info e #'#%index-result))]
   [last PairList.last
         (lambda (e)
           (syntax-local-static-info e #'#%index-result))]
   [rest PairList.rest
         (lambda (e)
           (define maybe-index-result
             (syntax-local-static-info e #'#%index-result))
           (if maybe-index-result
               #`((#%index-result #,maybe-index-result)
                  . #,list-static-infos)
               list-static-infos))])
  #:methods
  (length
   get
   reverse
   append
   take
   take_last
   drop
   drop_last
   has_element
   find
   remove
   map
   for_each
   sort
   to_list))

(define-name-root NonemptyList
  #:fields
  ([of NonemptyList.of]))

(define-name-root NonemptyPairList
  #:fields
  ([of NonemptyPairList.of]))

(define-binding-syntax List.cons
  (binding-transformer
   (let ([composite (make-composite-binding-transformer
                     "List.cons" #'nonempty-treelist? (list #'(lambda (l) (treelist-ref l 0))) (list #'())
                     #:static-infos treelist-static-infos
                     #:index-result-info? #t
                     #:rest-accessor #'(lambda (l) (treelist-drop l 1))
                     #:rest-to-repetition #'treelist->list
                     #:rest-repetition? #f)])
     (lambda (tail)
       (syntax-parse tail
         [(form-id (tag::parens elem list) . new-tail)
          (composite #'(form-id (tag elem) . new-tail)
                     #`(#,group-tag rest-bind #,treelist-static-infos
                        #:annot-prefix? #f
                        list))])))))

(define-binding-syntax PairList.cons
  (binding-transformer
   (let ([composite (make-composite-binding-transformer
                     "PairList.cons" #'nonempty-list? (list #'car) (list #'())
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

(set-primitive-contract! 'list? "PairList")
(set-primitive-contract! 'treelist? "List")

(define (check-treelist who l)
  (unless (treelist? l)
    (raise-argument-error* who rhombus-realm "List" l)))

(define (check-list who l)
  (unless (list? l)
    (raise-argument-error* who rhombus-realm "PairList" l)))

(define/arity (List.cons a d)
  #:inline
  #:primitive (treelist-cons)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-cons d a))

(define/arity (PairList.cons a d)
  #:inline
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who d)
  (cons a d))

(define/method (List.add d a)
  #:inline
  #:primitive (treelist-add)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-add d a))

(define/method (List.insert d pos a)
  #:inline
  #:primitive (treelist-insert)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-insert d pos a))

(define/method (List.delete d pos)
  #:inline
  #:primitive (treelist-delete)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-delete d pos))

(define (nonempty-treelist? l)
  (and (treelist? l) ((treelist-length l) . > . 0)))

(define (check-nonempty-treelist who l)
  (unless (nonempty-treelist? l)
    (raise-argument-error* who rhombus-realm "NonemptyList" l)))

(define (check-nonempty-list who l)
  (unless (and (pair? l) (list? l))
    (raise-argument-error* who rhombus-realm "NonemptyPairList" l)))

(define/arity (List.first l)
  #:primitive (treelist-first)
  (treelist-first l))

(define/arity (List.last l)
  #:primitive (treelist-last)
  (treelist-last l))

(define/arity (List.rest l)
  #:primitive (treelist-rest)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-rest l))

(define/arity (PairList.first l)
  #:inline
  (check-nonempty-list who l)
  (car l))

(define/arity (PairList.last l)
  #:inline
  (check-nonempty-list who l)
  (let loop ([l l])
    (if (null? (cdr l))
        (car l)
        (loop (cdr l)))))

(define/arity (PairList.rest l)
  #:inline
  #:static-infos ((#%call-result #,list-static-infos))
  (check-nonempty-list who l)
  (cdr l))

(define (check-nonneg-int who n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error* who rhombus-realm "NonnegInt" n)))

(define/arity (List.iota n)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-nonneg-int who n)
  (for/treelist ([i (in-range n)])
    i))

(define/arity (PairList.iota n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-nonneg-int who n)
  (for/list ([i (in-range n)])
    i))

(define/method (List.length l)
  #:inline
  #:primitive (treelist-length)
  #:static-infos ((#%call-result #,int-static-infos))
  (treelist-length l))

(define/method (PairList.length l)
  #:inline
  #:primitive (length)
  #:static-infos ((#%call-result #,int-static-infos))
  (length l))

(define/method (List.reverse l)
  #:primitive (treelist-reverse)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-reverse l))

(define/method (PairList.reverse l)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (reverse l))

;; used to define `List` and `PairList` further below:
(define-for-syntax (make-constructor proc-stx build-form static-infos wrap-static-infos
                                     #:repetition? [repetition? #f]
                                     #:convert-rep [convert-rep #f])
  ;; special cases optimize for `...` and `&`; letting it expand
  ;; instead to `(apply list ....)` is not so bad, but we can
  ;; avoid a `list?` check in `apply`, and we can expose more static
  ;; information this way
  (lambda (stx)
    (syntax-parse stx
      #:datum-literals (group)
      [(form-id (tag::parens _ ... _ (group _::...-expr)) . tail)
       #:when (normal-call? #'tag)
       (parse-*list-form stx build-form static-infos wrap-static-infos
                         #:convert-rep convert-rep
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(form-id (tag::parens _ ... (group _::&-expr _ ...)) . tail)
       #:when (normal-call? #'tag)
       (parse-*list-form stx build-form static-infos wrap-static-infos
                         #:convert-rep convert-rep
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(form-id (tag::brackets _ ...) . tail)
       (parse-*list-form stx build-form static-infos wrap-static-infos
                         #:convert-rep convert-rep
                         #:repetition? repetition?
                         #:span-form-name? #t)]
      [(_ . tail)
       (values proc-stx #'tail)])))

(define-for-syntax (make-treelist-rest-selector args-n for-rep?)
  (if (= 0 args-n)
      #'values
      #`(lambda (v) (treelist-drop v '#,args-n))))

(define-for-syntax (make-list-rest-selector args-n for-rep?)
  (if (= 0 args-n)
      #'values
      #'cdr))

(define-for-syntax (make-binding generate-binding make-rest-selector static-infos)
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id (_::parens arg ...) . tail)
        (parse-*list-binding stx generate-binding make-rest-selector static-infos)]
       [(form-id (_::brackets arg ...) . tail)
        (parse-*list-binding stx generate-binding make-rest-selector static-infos)]))))

(define-for-syntax (parse-list-binding stx)
  (parse-*list-binding stx generate-treelist-binding make-treelist-rest-selector treelist-static-infos))

(define-annotation-constructor (List List.of)
  ()
  #'treelist? treelist-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-treelist #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'treelist-build-convert #'())

(define-annotation-constructor (PairList PairList.of)
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

(define-syntax (treelist-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst empty-treelist])
              ([v (in-treelist #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (treelist-add lst v))
       (lambda () #f))))

(define-syntax (list-build-convert arg-id build-convert-stxs kws data)
  #`(for/fold ([lst '()] #:result (and lst (reverse lst)))
              ([v (in-list #,arg-id)])
      #:break (not lst)
      (#,(car build-convert-stxs)
       v
       (lambda (v) (cons v lst))
       (lambda () #f))))

(define-static-info-syntax empty-treelist
  #:defined treelist-static-infos)

(define-static-info-syntax null
  #:defined list-static-infos)

(define-binding-syntax empty-treelist
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'treelist-empty-infoer #'()) #'tail)]))))

(define-binding-syntax null
  (binding-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id . tail)
        (values (binding-form #'empty-infoer #'()) #'tail)]))))

(define-syntax (treelist-empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "List.empty"
                   #'empty
                   (static-infos-union treelist-static-infos #'up-static-infos)
                   #'()
                   #'treelist-empty-matcher
                   #'literal-bind-nothing
                   #'literal-commit-nothing
                   #'datum)]))

(define-syntax (empty-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos _)
     (binding-info "PairList.empty"
                   #'empty
                   (static-infos-union list-static-infos #'up-static-infos)
                   #'()
                   #'empty-matcher
                   #'literal-bind-nothing
                   #'literal-commit-nothing
                   #'datum)]))

(define-syntax (treelist-empty-matcher stx)
  (syntax-parse stx
    [(_ arg-id datum IF success fail)
     #'(IF (treelist-empty? arg-id)
           success
           fail)]))

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
  #'nonempty-treelist? treelist-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-treelist #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'treelist-build-convert #'())

(define-annotation-constructor (NonemptyPairList NonemptyPairList.of)
  ()
  #'nonempty-list? list-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-list #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%index-result #,(car static-infoss))))
  #'list-build-convert #'())

(define-reducer-syntax List
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer/no-break #'build-from-root
                                  #'([root unsafe-empty-root]
                                     [size 0]
                                     [height 0])
                                  #'build-treelist-accum
                                  treelist-static-infos
                                  #'(root size height))
                #'tail)]))))

(define-syntax (build-treelist-accum stx)
  (syntax-parse stx
    [(_ (root size height) e) #'(unsafe-root-add root size height e)]))

(define-syntax (build-from-root stx)
  (syntax-parse stx
    [(_ _ e) #'(let-values ([(root size height) e])
                 (unsafe-treelist root size height))]))

(define-reducer-syntax PairList
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

(define-syntax (build-identity stx)
  (syntax-parse stx
    [(_ accum e) #'e]))

(define-syntax (build-accum stx)
  (syntax-parse stx
    [(_ accum e) #'(cons e accum)]))

(define-for-syntax (make-repet g-to-list-stx)
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and args (_::parens g)) . tail)
        (values (make-repetition-info #'(form-id args)
                                      #'repet
                                      (g-to-list-stx #'g)
                                      1
                                      0
                                      #'()
                                      #f)
                #'tail)]))))

(define-repetition-syntax List.repet
  (make-repet (lambda (g)
                #`(let ([l (rhombus-expression #,g)])
                    (check-treelist 'List.repet l)
                    (treelist->list l)))))

(define-repetition-syntax PairList.repet
  (make-repet (lambda (g)
                #`(let ([l (rhombus-expression #,g)])
                    (check-list 'PairList.repet l)
                    l))))

(define (check-function-of-arity n who proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc n))
    (raise-argument-error* who rhombus-realm
                           (string-append "Function.of_arity("
                                          (number->string n)
                                          ")")
                           proc)))

(define/method (List.map lst proc)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-treelist who lst)
  (check-function-of-arity 1 who proc)
  (for/treelist ([e (in-treelist lst)])
    (proc e)))

(define/method (List.for_each lst proc)
  (check-treelist who lst)
  (check-function-of-arity 1 who proc)
  (for ([e (in-treelist lst)])
    (proc e)))

(define/method (PairList.map lst proc)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (map proc lst))

(define/method (PairList.for_each lst proc)
  (check-list who lst)
  (check-function-of-arity 1 who proc)
  (for-each proc lst))

(define/method (List.sort lst [less-than? <])
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-treelist who lst)
  (check-function-of-arity 2 who less-than?)
  (vector->treelist (vector-sort (treelist->vector lst) less-than?)))

(define/method (PairList.sort lst [less-than? <])
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who lst)
  (check-function-of-arity 2 who less-than?)
  (sort lst less-than?))

(define/method (List.to_list lst)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-treelist who lst)
  lst)

(define/method (PairList.to_list lst)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-list who lst)
  (list->treelist lst))

(define/method (List.get l n)
  #:inline
  #:primitive (treelist-ref)
  (treelist-ref l n))

(define/method (List.set l n v)
  #:inline
  #:primitive (treelist-set)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-set l n v))

(define/method List.append
  #:inline
  #:primitive (treelist-append)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (case-lambda
    [() empty-treelist]
    [(a) (treelist-append a)]
    [(a b) (treelist-append a b)]
    [(a b c) (treelist-append a b c)]
    [as (apply treelist-append as)]))

;; only check that the *last* argument is list here, since `append` checks the rest
(define/method PairList.append
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
       (raise-argument-error* who rhombus-realm "PairList" b))
     (append a b)]
    [ls
     (let ([ln (let loop ([ls ls])
                 (if (pair? (cdr ls)) (loop (cdr ls)) (car ls)))])
       (unless (list? ln)
         (for ([l (in-list ls)])
           (check-list who l))
         (raise-argument-error* who rhombus-realm "PairList" ln)))
     (apply append ls)]))

;; primitive doesn't check for listness
(define/method (PairList.get l n)
  #:inline
  #:primitive (list-ref)
  (check-list who l)
  (list-ref l n))

(define/method (List.take l n)
  #:inline
  #:primitive (treelist-take)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-take l n))

(define/method (List.take_last l n)
  #:inline
  #:primitive (treelist-take-right)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-take-right l n))

(define/method (List.drop l n)
  #:inline
  #:primitive (treelist-drop)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-drop l n))

(define/method (List.drop_last l n)
  #:inline
  #:primitive (treelist-drop-right)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (treelist-drop-right l n))

(define/method (List.sublist l start end)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-treelist who l)
  (define len (treelist-length l))
  (unless (and (0 . <= . start) (start . <= . len))
    (raise-range-error* who rhombus-realm "list" "starting " start l 0 len))
  (unless (and (start . <= . end) (end . <= . len))
    (raise-range-error* who rhombus-realm "list" "ending " end l start len))
  (treelist-take (treelist-drop l start) (- end start)))

(define (raise-list-count who what l len n)
  (raise-arguments-error* who rhombus-realm
                          (string-append "list is shorter than the number of elements to "
                                         what)
                          "list length" (unquoted-printing-string (number->string len))
                          (string-append "number to " what)
                          (unquoted-printing-string (number->string n))))

(define/method (PairList.take orig-l orig-n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who orig-l)
  (check-nonneg-int who orig-n)
  (let loop ([l orig-l] [n orig-n])
    (cond
      [(zero? n) null]
      ;; too many to take
      [(null? l) (raise-list-count who "take" orig-l (- orig-n n) orig-n)]
      [else (cons (car l) (loop (cdr l) (sub1 n)))])))

(define/method (PairList.take_last l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (check-nonneg-int who n)
  (define len (length l))
  (when (n . > . len)
    (raise-list-count who "take" l len n))
  (list-tail l (- len n)))

(define/method (PairList.drop orig-l orig-n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who orig-l)
  (check-nonneg-int who orig-n)
  (let loop ([l orig-l] [n orig-n])
    (cond
      [(zero? n) l]
      ;; too many to drop
      [(null? l) (raise-list-count who "drop" orig-l (- orig-n n) orig-n)]
      [else (loop (cdr l) (sub1 n))])))

(define/method (PairList.drop_last l n)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (check-nonneg-int who n)
  (define len (length l))
  (when (n . > . len)
    (raise-list-count who "drop" l len n))
  (for/list ([a (in-list l)]
             [i (in-range 0 (- len n))])
    a))

(define/method (List.remove l v)
  #:primitive (treelist-remove)
  #:static-infos ((#%call-result #,treelist-static-infos))
  (check-treelist who l)
  (define len (treelist-length l))
  (let loop ([i 0])
    (cond
      [(= i len) l]
      [(equal-always? v (treelist-ref l i))
       (treelist-delete l i)]
      [else (loop (+ i 1))])))

(define/method (PairList.remove l v)
  #:static-infos ((#%call-result #,list-static-infos))
  (check-list who l)
  (remove v l equal-always?))

(define/method (List.has_element l v [eql equal-always?])
  #:primitive (treelist-member?)
  (treelist-member? l v eql))

(define/method (PairList.has_element l v [eql equal-always?])
  (check-list who l)
  (check-function-of-arity 2 who eql)
  (and (member v l eql) #t))

(define/method (List.find l pred)
  #:primitive (treelist-find)
  (treelist-find l pred))

(define/method (PairList.find l pred)
  (check-list who l)
  (check-function-of-arity 1 who pred)
  (findf pred l))

(define-for-syntax (wrap-treelist-static-info expr)
  (wrap-static-info* expr treelist-static-infos))

(define-for-syntax (wrap-list-static-info expr)
  (wrap-static-info* expr list-static-infos))

;; parses a list pattern that has already been checked for use with a
;; suitable `parens` or `brackets` form
(define-for-syntax (parse-*list-binding stx generate-binding make-rest-selector static-infos)
  (syntax-parse stx
    #:datum-literals (group)
    [(form-id (_ arg ... (group _::&-bind rest-arg ...)) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (generate-binding #'form-id len #t args #'tail
                       #`(#,group-tag rest-bind #,static-infos
                          (#,group-tag rest-arg ...))
                       (make-rest-selector len #f)
                       #f)]
    [(form-id (_ arg ... rest-arg (group _::...-bind)) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (generate-binding #'form-id len #t args #'tail #'rest-arg
                       (make-rest-selector len #t)
                       #t)]
    [(form-id (_ arg ...) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (generate-binding #'form-id len #f args #'tail)]))

(define-for-syntax (generate-treelist-binding form-id len or-more? args tail [rest-arg #f] [rest-selector #f]
                                              [rest-repetition? #t])
  (define pred #`(lambda (v)
                   (and (treelist? v)
                        (#,(if or-more? #'>= #'=) (treelist-length v) #,len))))
  ((make-composite-binding-transformer "List"
                                       pred
                                       (for/list ([i (in-range (length args))])
                                         #`(lambda (tl) (treelist-ref tl #,i)))
                                       (for/list ([arg (in-list args)])
                                         #'())
                                       #:index-result-info? #t
                                       #:rest-accessor rest-selector
                                       #:rest-repetition? rest-repetition?
                                       #:rest-to-repetition #'treelist->list
                                       #:static-infos treelist-static-infos)
   #`(#,form-id (parens . #,args) . #,tail)
   rest-arg))

(define-for-syntax (generate-list-binding form-id len or-more? args tail [rest-arg #f] [rest-selector #f]
                                          [rest-repetition? #t])
  (define pred #`(lambda (v)
                   (and (list? v)
                        #,(let ([check #`(maybe-list-tail v #,len)])
                            (if or-more?
                                #`(and #,check #t)
                                #`(null? #,check))))))
  ((make-composite-binding-transformer "PairList"
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

(define-binding-syntax List (make-binding generate-treelist-binding make-treelist-rest-selector treelist-static-infos))
(define-binding-syntax PairList (make-binding generate-list-binding make-list-rest-selector list-static-infos))

(begin-for-syntax
  (struct list-rest (syntax))
  (struct list-rest-splice list-rest ())
  (struct list-rest-rep list-rest ()))

(define-for-syntax (parse-*list-form stx
                                     build-form
                                     static-infos
                                     wrap-static-info
                                     #:convert-rep convert-rep
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
            [repetition? (repetition-as-deeper-repetition
                          seq static-infos
                          #:convert convert-rep)]
            [(not convert-rep) (wrap-static-info seq)]
            [else (wrap-static-info
                   ;; rotate static info for `content` out to converted form
                   (let ([content-static-infos (extract-static-infos seq)])
                     (wrap-static-info*
                      #`(#,convert-rep #,(unwrap-static-infos seq))
                      content-static-infos)))])]
         [(not repetition?)
          (wrap-static-info
           (tag-props
            (build-form content)))]
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
               (values (tag-props (build-form content))
                       static-infos))))]))
      #'tail)]))

(define-for-syntax (build-*list-form content *list-stx empty-*list-stx *list-append-stx
                                     ensure-*list-stx list->*list-stx)
  ;; group content into a list of list-generating groups,
  ;; and those lists will be appended
  (define (gather group groups)
    (if (null? group)
        groups
        (cons #`(#,*list-stx #,@group) groups)))
  (define groups
    (for/foldr ([group '()]
                [groups '()]
                #:result (gather group groups))
               ([one (in-list content)])
      (cond
        [(list-rest-splice? one)
         (values '()
                 (cons #`(#,ensure-*list-stx #,(list-rest-syntax one))
                       (gather group groups)))]
        [(list-rest-rep? one)
         (values '()
                 (cons (let ([lst (list-rest-syntax one)])
                         (if list->*list-stx
                             #`(#,list->*list-stx #,lst)
                             lst))
                       (gather group groups)))]
        [else (values (cons one group) groups)])))
  (cond
    [(null? groups) empty-*list-stx]
    [(null? (cdr groups)) (car groups)]
    [else #`(#,*list-append-stx #,@groups)]))

(define-for-syntax (build-treelist-form content)
  (build-*list-form content #'treelist #'empty-treelist #'treelist-append
                    #'ensure-treelist #'list->treelist))

(define-for-syntax (build-list-form content)
  (build-*list-form content #'list #'null #'append
                    #'ensure-list #f))

(define-syntax List
  (expression-transformer
   (make-constructor #'treelist build-treelist-form treelist-static-infos wrap-treelist-static-info
                     #:convert-rep #'list->treelist/optimize)))
(define-syntax PairList
  (expression-transformer
   (make-constructor #'list build-list-form list-static-infos wrap-list-static-info)))

(define-repetition-syntax List
  (repetition-transformer
   (make-constructor #:repetition? #t
                     #'treelist build-treelist-form treelist-static-infos wrap-treelist-static-info
                     #:convert-rep #'list->treelist/optimize)))
(define-repetition-syntax PairList
  (repetition-transformer
   (make-constructor #:repetition? #t
                     #'list build-list-form list-static-infos wrap-list-static-info)))

(define-for-syntax (parse-list-expression stx)
  (parse-*list-form stx build-treelist-form treelist-static-infos wrap-treelist-static-info
                    #:convert-rep #'list->treelist/optimize
                    #:repetition? #f
                    #:span-form-name? #f))

(define-for-syntax (parse-list-repetition stx)
  (parse-*list-form stx build-treelist-form treelist-static-infos wrap-treelist-static-info
                    #:convert-rep #'list->treelist/optimize
                    #:repetition? #t
                    #:span-form-name? #f))

(define-syntax (list->treelist/optimize stx)
  (syntax-parse stx
    #:literals (treelist->list)
    [(_ (treelist->list t)) #'t]
    [(_ e) #'(list->treelist e)]
    [_ #'list->treelist]))

(define (maybe-list-tail l n)
  (or (and (eqv? n 0)
           l)
      (and (pair? l)
           (maybe-list-tail (cdr l) (sub1 n)))))

(define-values (prop:Listable Listable? Listable-ref)
  (make-struct-type-property 'Listable))

(define (listable? v)
  (or (treelist? v)
      (list? v)
      (vector? v)
      (Listable? v)))

(define (to-treelist who v)
  (cond
    [(treelist? v) v]
    [(list? v) (list->treelist v)]
    [(vector? v) (vector->treelist v)]
    [else (general-to-treelist who v)]))

(define (to-list who v)
  (cond
    [(treelist? v) (treelist->list v)]
    [(list? v) v]
    [(vector? v) (vector->list v)]
    [(general-to-treelist who v) => treelist->list]
    [else #f]))

(define (general-to-treelist who v)
  (define methods (Listable-ref v #f))
  (cond
    [(not methods)
     (and who
          (raise-argument-error* who rhombus-realm "Listable" v))]
    [else
     (define lst ((vector-ref methods 0) v))
     ;; guarded by method result
     lst]))

(define (ensure-treelist v)
  (or (to-treelist #f v)
      (raise-arguments-error* 'List rhombus-realm
                              "not a listable for splicing into a pair list"
                              "value" v)))

(define (ensure-list v)
  (or (to-list #f v)
      (raise-arguments-error* 'PairList rhombus-realm
                              "not a listable for splicing into a pair list"
                              "value" v)))

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
