#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
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
         (only-in "ellipsis.rkt"
                  [... rhombus...])
         (only-in "rest-marker.rkt"
                  &)
         "repetition.rkt"
         "name-root.rkt"
         (submod "dot.rkt" for-dot-provider)
         "parse.rkt"
         "dot-parse.rkt"
         "realm.rkt"
         "parens.rkt")

(provide List
         (for-space rhombus/annotation List)
         (for-space rhombus/reducer List)
         (for-space rhombus/repetition List)

         (for-space rhombus/annotation NonemptyList))

(module+ for-binding
  (provide (for-syntax parse-list-binding
                       parse-list-expression
                       parse-list-repetition)))

(module+ for-builtin
  (provide list-method-table))

(module+ for-implicit
  (provide (for-syntax set-#%call-id!)))

(define list-method-table
  (hash 'length (method1 length)
        'first car
        'rest cdr))

(define-for-syntax list-static-infos
  #'((#%map-ref list-ref)
     (#%sequence-constructor in-list)
     (#%dot-provider list-instance)))

(define-binding-syntax List.cons
  (binding-transformer
   #'List.cons
   (make-composite-binding-transformer "cons" #'list? (list #'car #'cdr) (list #'() list-static-infos))))

(define (List.cons a d)
  (unless (list? d) (raise-argument-error* 'List.cons rhombus-realm "List" d))
  (cons a d))

(define (first l)
  (unless (list? l) (raise-argument-error* 'List.first rhombus-realm "List" l))
  (car l))

(define (rest l)
  (unless (and (pair? l) (list? l)) (raise-argument-error* 'List.rest rhombus-realm "NonemptyList" l))
  (cdr l))

(define-name-root List
  #:fields
  (length
   [cons List.cons]
   first
   rest
   [empty null])
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
     (syntax-parse stx
       #:datum-literals (parens group op)
       #:literals (rhombus... &)
       [(form-id (tag::parens _ ... _ (group (op rhombus...))) . tail)
        (free-identifier=? #%call-id (datum->syntax #'tag '#%call))
        (parse-list-expression stx)]
       [(form-id (tag::parens _ ... (group (op &) _ ...)) . tail)
        (free-identifier=? #%call-id (datum->syntax #'tag '#%call))
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
        [else #f])))))

(define-reducer-syntax List
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        #`[reverse
           ([accum null])
           ((lambda (v) (cons v accum)))
           #,list-static-infos]]))))

(define-name-root List
  #:space rhombus/repetition
  #:fields
  (repet))

(define-syntax repet
  (repetition-transformer
   #'List
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (op |.| parens group repet)
       [(form-id (parens g) . tail)
        (define name (syntax-e #'form-id))
        (values (make-repetition-info name
                                      #`(check-repetition-list '#,name (rhombus-expression g))
                                      1
                                      0
                                      #'())
                #'tail)]))))

(define (check-repetition-list who v)
  (unless (list? v)
    (raise-argument-error who "List" v))
  v)

(define-static-info-syntax list
  (#%call-result #,list-static-infos))

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

(define-for-syntax (parse-list-expression stx)
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (& rhombus...)
    [(form-id (tag arg ...) . tail)
     #:when (complex-argument-splice? #'(arg ...))
     ;; general case: multiple `...` or `&`, or at least one of those not at the end:
     (let loop ([gs-stx #'(arg ...)] [accum-values '()] [accum-lists '()])
       (define (build-list)
         (with-syntax ([(arg ...) (reverse accum-values)])
           #`(list arg ...)))
       (syntax-parse gs-stx
         #:datum-literals (group op)
         #:literals (& rhombus...)
         [()
          (cond
            [(null? accum-values)
             (with-syntax ([(lst ...) (reverse accum-lists)])
               (values (wrap-list-static-info
                        (quasisyntax/loc #'tag
                          (append lst ...)))
                       #'tail))]
            [else (loop gs-stx '() (cons (build-list) accum-lists))])]
         [((group (op &) rand ...+) . gs)
          (cond
            [(null? accum-values)
             (loop #'gs '() (cons #'(assert-list (rhombus-expression (group rand ...))) accum-lists))]
            [else (loop gs-stx '() (cons (build-list) accum-lists))])]
         [(rep-arg (group (op rhombus...)) . gs)
          (cond
            [(null? accum-values)
             (loop #'gs '() (cons (repetition-as-list #'ellipses #'rep-arg 1) accum-lists))]
            [else (loop gs-stx '() (cons (build-list) accum-lists))])]
         [(g . gs) (loop #'gs (cons #'(rhombus-expression g) accum-values) accum-lists)]))]
    [(form-id (tag arg ... (group (op &) rest-arg ...)) . tail)
     (values (wrap-list-static-info
              (cond
                [(null? (syntax->list #'(arg ...)))
                 ;; special case to expose static info on rest elements
                 (quasisyntax/loc #'tag
                   (rhombus-expression (group rest-arg ...)))]
                [else
                 (quasisyntax/loc #'tag
                   (list* (rhombus-expression arg) ...
                          (assert-list (rhombus-expression (group rest-arg ...)))))]))
             #'tail)]
    [(form-id (tag arg ... rep-arg (group (op (~and ellipses rhombus...)))) . tail)
     (values (wrap-list-static-info
              (cond
                [(null? (syntax->list #'(arg ...)))
                 ;; special case to expose static info on rest elements
                 (quasisyntax/loc #'tag
                   #,(repetition-as-list #'ellipses #'rep-arg 1))]
                [else
                 (quasisyntax/loc #'tag
                   (list* (rhombus-expression arg) ... #,(repetition-as-list #'ellipses #'rep-arg 1)))]))
             #'tail)]
    [(form-id (tag arg ...) . tail)
     (values (wrap-list-static-info
              (syntax/loc #'tag
                (list (rhombus-expression arg) ...)))
             #'tail)]))

(define-for-syntax (parse-list-repetition stx)
  (syntax-parse stx
    #:datum-literals (group op)
    #:literals (rhombus...)
    [(form-id (tag rep::repetition (group (op (~and ellipses rhombus...)))) . tail)
     #:with rep-info::repetition-info #'rep.parsed
     (values (make-repetition-info #'rep-info.name
                                   #'rep-info.seq-id
                                   #'rep-info.bind-depth
                                   (+ (syntax-e #'rep-info.use-depth) 1)
                                   #'rep-info.element-static-infos)
             #'tail)]))

(define-for-syntax (complex-argument-splice? gs-stx)
  (syntax-parse gs-stx
    #:datum-literals (group op)
    #:literals (& rhombus...)
    [() #f]
    [((group (op &) rand ...+) g . _) #t]
    [(g0 (group (op rhombus...)) g . _) #t]
    [(_ . gs) (complex-argument-splice? #'gs)]))

(define (assert-list v)
  (unless (list? v)
    (raise-arguments-error* 'List rhombus-realm
                            "not a set for splicing"
                           "value" v))
  v)

(begin-for-syntax
  (define #%call-id #f)
  (define (set-#%call-id! id) (set! #%call-id id)))
