#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/stx)
         "composite.rkt"
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         (submod "annotation.rkt" for-class)
         "static-info.rkt"
         "reducer.rkt"
         "map-ref-set-key.rkt"
         "call-result-key.rkt"
         "ref-result-key.rkt"
         "function-arity-key.rkt"
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         (only-in "rest-marker.rkt"
                  &)
         "repetition.rkt"
         "compound-repetition.rkt"
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parse.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "parens.rkt"
         "define-arity.rkt")

(provide List
         (for-space rhombus/annot List)
         (for-space rhombus/reducer List)

         (for-space rhombus/annot NonemptyList))

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

(define list-method-table
  (hash 'length (method1 length)
        'first car
        'rest cdr
        'reverse (method1 reverse)))

(define-for-syntax list-static-infos
  #'((#%map-ref list-ref)
     (#%sequence-constructor in-list)
     (#%dot-provider list-instance)))

(define-binding-syntax List.cons
  (binding-transformer
   #'List.cons
   (make-composite-binding-transformer "cons" #'list? (list #'car #'cdr) (list #'() list-static-infos))))

(define/arity (List.cons a d)
  (unless (list? d) (raise-argument-error* 'List.cons rhombus-realm "List" d))
  (cons a d))

(define/arity (first l)
  (unless (list? l) (raise-argument-error* 'List.first rhombus-realm "List" l))
  (car l))

(define/arity (rest l)
  (unless (and (pair? l) (list? l)) (raise-argument-error* 'List.rest rhombus-realm "NonemptyList" l))
  (cdr l))

(define (iota n)
  (unless (exact-nonnegative-integer? n)
    (raise-argument-error* 'List.iota rhombus-realm "NonnegativeInteger" n))
  (for/list ([i (in-range n)])
    i))

(define-static-info-syntax length
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
   repet)
  #:root
  (make-expression+binding-prefix-operator
   #'List
   '((default . stronger))
   'macro
   ;; expression - special cases optimize for `...` and `&`;
   ;; letting it expand instead to `(apply list ....)` is not
   ;; so bad, but but we can avoid a `list?` check in `apply`,
   ;; and we can expose more static information this way
   (lambda (stx)
     (define (normal-call? tag)
       (define id (datum->syntax tag '#%call))
       (or (free-identifier=? #%call-id id)
           (free-identifier=? static-#%call-id id)))
     (syntax-parse stx
       #:datum-literals (parens group op)
       #:literals (rhombus... &)
       [(form-id (tag::parens _ ... _ (group (op rhombus...))) . tail)
        #:when (normal-call? #'tag)
        (parse-list-expression stx)]
       [(form-id (tag::parens _ ... (group (op &) _ ...)) . tail)
        #:when (normal-call? #'tag)
        (parse-list-expression stx)]
       [(_ . tail)
        (values #'list #'tail)]))
   ;; binding
   (lambda (stx)
     (syntax-parse stx
       [(form-id ((~and tag (~datum parens)) arg ...) . tail)
        (parse-list-binding stx)]))))

(define-annotation-constructor List
  ()
  #'list? list-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-list #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%ref-result #,(car static-infoss)))))

(define (nonempty-list? l)
  (and (pair? l) (list? l)))

(define-annotation-constructor NonemptyList
  ()
  #'nonempty-list? list-static-infos
  1
  #f
  (lambda (arg-id predicate-stxs)
    #`(for/and ([e (in-list #,arg-id)])
        (#,(car predicate-stxs) e)))
  (lambda (static-infoss)
    #`((#%ref-result #,(car static-infoss)))))

(define-syntax list-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(length) (0ary #'length)]
        [(first) (field (lambda (e)
                          (wrap-static-info* #`(car #,e)
                                             (or (syntax-local-static-info e #'#%ref-result)
                                                 #'()))))]
        [(rest) (field (lambda (e)
                         (wrap-static-info* #`(cdr #,e)
                                            (datum->syntax #f
                                                           (or (extract-static-infos e)
                                                               '())))))]
        [(reverse) (0ary #'reverse)]
        [else (fail-k)])))))

(define-reducer-syntax List
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        #`[reverse
           ([accum null])
           ((lambda (v) (cons v accum)))
           #,list-static-infos]]))))

(define-syntax repet
  (repetition-transformer
   #'List
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op |.| parens group repet)
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
                                         #:ref-result-info? #t
                                         #:rest-accessor rest-selector
                                         #:rest-repetition? rest-repetition?
                                         #:static-infos list-static-infos)
     #`(#,form-id (parens . #,args) . #,tail)
     rest-arg))
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (& rhombus...)
    [(form-id (_ arg ... (group (op &) rest-arg ...)) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (>= (length v) #,len))))
     (generate-binding #'form-id pred args #'tail #'(group rest-arg ...)
                       (if (null? args) #'values #'cdr)
                       #f)]
    [(form-id (_ arg ... rest-arg (group (op rhombus...))) . tail)
     (define args (syntax->list #'(arg ...)))
     (define len (length args))
     (define pred #`(lambda (v)
                      (and (list? v)
                           (>= (length v) #,len))))
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

(define-for-syntax (parse-list-form stx #:repetition? repetition?)
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (& rhombus...)
    [(form-id (tag arg ...) . tail)
     ;; a list of syntax, (cons 'splice syntax), and (cons 'seq syntax):
     (define content
       (let loop ([gs-stx #'(arg ...)] [accum '()])
         (syntax-parse gs-stx
           #:datum-literals (group op)
           #:literals (& rhombus...)
           [() (reverse accum)]
           [((group (op &) rand ...+) . gs)
            (define e (let ([g #'(group rand ...)])
                        (if repetition?
                            (syntax-parse g
                              [rep::repetition #'rep.parsed])
                            (syntax-parse g
                              [e::expression #'e.parsed]))))
            (loop #'gs (cons (list 'splice e) accum))]
           [(rep-arg (group (op rhombus...)) . gs)
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
     (values
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
          (build-list-form content))]
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
              (values (build-list-form content)
                      list-static-infos))))])
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
  (parse-list-form stx #:repetition? #f))

(define-for-syntax (parse-list-repetition stx)
  (parse-list-form stx #:repetition? #t))

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
    (set! static-#%call-id static-id)))
