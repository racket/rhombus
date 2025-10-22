#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     syntax/strip-context
                     shrubbery/property
                     "consistent.rkt"
                     "entry-point-adjustment.rkt"
                     "dotted-sequence.rkt"
                     "srcloc.rkt"
                     "annot-context.rkt")
         racket/keyword
         racket/treelist
         "../version-case.rkt"
         "provide.rkt"
         (submod "function-parse.rkt" for-build)
         (submod "list.rkt" for-compound-repetition)
         "simple-call.rkt"
         (submod "implicit.rkt" normal-literal)
         "parens.rkt"
         "op-literal.rkt"
         (submod "ellipsis.rkt" for-parse)
         "expression.rkt"
         "repetition.rkt"
         "definition.rkt"
         "entry-point.rkt"
         "immediate-callee.rkt"
         "parse.rkt"
         "function-count.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "arithmetic.rkt" static-infos)
         (submod "symbol.rkt" for-static-info)
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         (submod "annotation.rkt" for-map-function)
         "dotted-sequence-parse.rkt"
         "realm.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "class-primitive.rkt"
         "rhombus-primitive.rkt"
         (submod "module.rkt" for-module+)
         (submod "arrow-annotation.rkt" for-arrow-annot)
         "name-prefix.rkt"
         "extract-name.rkt")

(provide (for-spaces (#f
                      rhombus/defn
                      rhombus/entry_point
                      rhombus/immediate_callee)
                     fun)
         (for-spaces (rhombus/namespace
                      rhombus/annot)
                     Function))

(module+ for-method
  (provide fun/read-only-property))

(module+ for-builtin
  (provide function-method-table))

(module+ for-info
  (provide (for-syntax get-function-static-infos)))

(define-primitive-class Function function procedure
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:just-annot
  #:fields ()
  #:namespace-fields
  (of_arity
   all_of
   [pass Function.pass/optimize]
   [black_box Function.black_box]
   [count Function.count])
  #:properties
  ()
  #:methods
  (map
   for_each
   name
   realm
   rename
   arity
   reduce_arity))

(define (check-proc who proc)
  (unless (procedure? proc)
    (raise-annotation-failure who proc "Function")))

(define (check-list who l)
  (unless (treelist? l)
    (raise-annotation-failure who l "List")))

(define (check-proc-arity who fn n)
  (define (make-msg why)
    (string-append "argument mismatch;\n " why))
  (define (keyword->string kw)
    (string-append "~" (keyword->immutable-string kw)))
  (define (single-arity->string a)
    (cond
      [(arity-at-least? a) (string-append
                            "at least "
                            (number->string (arity-at-least-value a)))]
      [else (number->string a)]))
  (define (list->string ->string sep lst)
    (cond
      [(null? (cdr lst))
       (->string (car lst))]
      [(null? (cddr lst))
       (string-append (->string (car lst))
                      " " sep " "
                      (->string (cadr lst)))]
      [else
       (for/foldr ([strs '()]
                   [idx 0]
                   #:result (apply string-append strs))
                  ([elem (in-list lst)])
         (values
          (case idx
            [(0) (cons (->string elem) strs)]
            [(1) (list* (->string elem) ", " sep " " strs)]
            [else (list* (->string elem) ", " strs)])
          (add1 idx)))]))
  (unless (procedure-arity-includes? fn n)
    (define-values (required-kws allowed-kws) (procedure-keywords fn))
    (cond
      [(pair? required-kws)
       (raise-arguments-error*
        who rhombus-realm
        (make-msg "the given function expects keyword arguments")
        "given function" fn
        "required keywords" (unquoted-printing-string
                             (list->string keyword->string "and" required-kws)))]
      [else
       (raise-arguments-error*
        who rhombus-realm
        (make-msg "the given function's expected number of arguments does not match the given number of lists")
        "given function" fn
        "expected" (unquoted-printing-string
                    (let ([a (procedure-arity fn)])
                      (cond
                        [(null? a) "nothing (cannot be called)"]
                        [(pair? a) (list->string single-arity->string "or" a)]
                        [else (single-arity->string a)])))
        "given" (unquoted-printing-string (number->string n)))])))

(define (check-list-length who fn l1 l2)
  (unless (= (treelist-length l1) (treelist-length l2))
    (raise-arguments-error* who rhombus-realm
                            "all lists must have same size"
                            "first list length" (unquoted-printing-string (number->string (treelist-length l1)))
                            "other list length" (unquoted-printing-string (number->string (treelist-length l2)))
                            "procedure" fn)))

(define-syntax (define-map stx)
  (syntax-parse stx
    [(_ Map for/treelist
        (~optional (~seq #:static-infos static-infos)))
     #:with method-name (datum->syntax #'Map (string->symbol (format "Function.~a" (syntax-e #'Map))))
     #'(define/method method-name
         (~? (~@ #:static-infos static-infos))
         (case-lambda
           [(fn lst)
            (check-proc who fn)
            (check-list who lst)
            (check-proc-arity who fn 1)
            (for/treelist ([e (in-treelist lst)])
              (fn e))]
           [(fn lst1 lst2)
            (check-proc who fn)
            (check-list who lst1)
            (check-list who lst2)
            (check-list-length who fn lst1 lst2)
            (check-proc-arity who fn 2)
            (for/treelist ([e1 (in-treelist lst1)]
                           [e2 (in-treelist lst2)])
              (fn e1 e2))]
           [(fn lst1 . lsts)
            (check-proc who fn)
            (check-list who lst1)
            (for ([lst (in-list lsts)])
              (check-list who lst)
              (check-list-length who fn lst1 lst))
            (check-proc-arity who fn (add1 (length lsts)))
            (for/treelist ([i (in-range (treelist-length lst1))])
              (apply fn
                     (treelist-ref lst1 i)
                     (for/list ([lst (in-list lsts)])
                       (treelist-ref lst i))))]))]))

(define-map map for/treelist
  #:static-infos ((#%call-result ((#%dependent-result (merge-map-result #f))))))
(define-map for_each for)

(define-syntax (merge-map-result data deps)
  (define args (annotation-dependencies-args deps))
  (define fun-i 0)
  (define fun-si (or (and (fun-i . < . (length args))
                          (list-ref args fun-i))
                     #'()))
  (define elem-si (or (extract-call-result fun-si) #'()))
  (if (not (static-infos-empty? elem-si))
      #`((#%index-result #,elem-si)
         #,@(get-treelist-static-infos))
      (get-treelist-static-infos)))

(define-for-syntax (wrap-function-static-info expr)
  (wrap-static-info* expr (get-function-static-infos)))

(define (handle-procedure-arity-includes/c form)
  (and (pair? (cdr form))
       (exact-nonnegative-integer? (cadr form))
       (null? (cddr form))
       (string-append "Function.of_arity(" (number->string (cadr form)) ")")))

(void (set-primitive-contract-combinator! 'procedure-arity-includes/c handle-procedure-arity-includes/c))
(define-annotation-syntax of_arity
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (syntax-parse stx
       [(form-id (~and args (_::parens g ...+)) . tail)
        (with-syntax ([(kw ...) (for/list ([g (in-list (syntax->list #'(g ...)))]
                                           #:do [(define kw
                                                   (syntax-parse g
                                                     [(_ kw:keyword) #'kw]
                                                     [_ #f]))]
                                           #:when kw)
                                  kw)]
                      [(g ...) (for/list ([g (in-list (syntax->list #'(g ...)))]
                                          #:unless (syntax-parse g
                                                     [(_ _:keyword) #t]
                                                     [_ #f]))
                                 g)])
          (with-syntax ([(kw-ok? kw-check) (let ([kws (syntax->list #'(kw ...))])
                                             (cond
                                               [(null? kws) #'(#f #t)]
                                               [else
                                                (for/fold ([ht #hasheq()]) ([kw-stx (in-list kws)])
                                                  (define kw (syntax-e kw-stx))
                                                  (when (hash-ref ht kw #f)
                                                    (raise-syntax-error #f "duplicate keyword" kw #f))
                                                  (hash-set ht kw #t))
                                                #`(#t (accepts-keywords? v '#,(sort (map syntax-e kws) keyword<?)))]))]
                        [(n ...) (generate-temporaries #'(g ...))]
                        [(function-arity-static ...) (syntax-parse #'(g ...)
                                                       #:datum-literals (group)
                                                       [((group n:exact-nonnegative-integer))
                                                        #:when (normal-literal? #'n)
                                                        #`((#%function-arity (#,(arithmetic-shift 1 (syntax-e #'n))
                                                                              ()
                                                                              (kw ...))))]
                                                       [_ #'()])])
            (values (relocate+reraw
                     (datum->syntax #f (list #'form-id #'args))
                     (annotation-predicate-form
                      #'(let ([n (rhombus-expression g)]
                              ...)
                          (check-nonneg-int 'Function.of_arity n)
                          ...
                          (lambda (v)
                            (and (procedure? v)
                                 (procedure-arity-includes? v n kw-ok?)
                                 ...
                                 kw-check)))
                      #`(function-arity-static ... . #,(get-function-static-infos))))
                    #'tail)))]))))

(define-annotation-syntax all_of
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (parse-arrow-all-of stx ctx))))

(define (check-nonneg-int who v)
  (unless (exact-nonnegative-integer? v)
    (raise-annotation-failure who v "NonnegInt")))

(define (accepts-keywords? proc kws)
  (define-values (req allow) (procedure-keywords proc))
  (define (kw-subset big small)
    (let loop ([big big] [small small])
      (cond
        [(null? small) #t]
        [(null? big) #f]
        [(eq? (car small) (car big)) (loop (cdr big) (cdr small))]
        [(keyword<? (car small) (car big)) #f]
        [else (loop (cdr big) small)])))
  (and (or (not allow)
           (kw-subset allow kws))
       (kw-subset kws req)))

(begin-for-syntax
  (define (get-local-name who adjustments)
    (or (entry-point-adjustment-name adjustments)
        (syntax-local-name)
        who))

  ;; check positional constraint on optional arguments
  ;; optional by-position arguments must follow required ones
  (define (check-optional-args stx args kws defaults)
    (for/fold ([seen-default? #f])
              ([arg (in-list args)]
               [kw (in-list kws)]
               [default (in-list defaults)]
               #:unless (syntax-e kw))
      (define default? (and (syntax-e default) #t))
      (when (and seen-default? (not default?))
        (raise-syntax-error #f
                            "default-value expression missing"
                            stx
                            arg))
      default?)
    (void))

  ;; check uniqueness constraint on keyword arguments
  (define (check-keyword-args stx args kws)
    (for/fold ([seen-kws #hasheq()])
              ([arg (in-list args)]
               [kw (in-list kws)]
               #:when (syntax-e kw))
      (when (hash-ref seen-kws (syntax-e kw) #f)
        (raise-syntax-error #f
                            "duplicate keyword for argument"
                            stx
                            arg
                            (list kw)))
      (hash-set seen-kws (syntax-e kw) #t))
    (void))

  (define (check-consistent-unsafes stx unsafe-kws)
    (when (ormap (lambda (kw) kw) unsafe-kws)
      (unless (andmap (lambda (kw) kw) unsafe-kws)
        (raise-syntax-error #f
                            "unsafe case present, but not every case has an unsafe block"
                            stx
                            (ormap (lambda (kw) kw) unsafe-kws)))))

  (define (check-args-for-unsafe stx args kw-ok?)
    (syntax-parse args
      [(_::parens arg ...)
       (for ([arg (syntax->list #'(arg ...))])
         (check-arg-for-unsafe stx arg kw-ok?))])))

(define-syntax fun
  (expression-transformer
   (lambda (stx)
     (parse-anonymous-function stx no-adjustments '()))))

(define-defn-syntax fun
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        #:datum-literals (group)
        ;; immediate alts case
        [(form-id (alts-tag::alts
                   (_::block
                    (group name-seq::dotted-identifier-sequence (~and args (_::parens arg::kw-binding ... rest::maybe-arg-rest))
                           (~var ret (:ret-annotation (parse-arg-context #'args)))
                           (~and rhs (_::block
                                      (~alt
                                       (~optional (~and who-clause (group #:who . _)))
                                       (~optional (~and unsafe (group (~and unsafe-kw #:unsafe) . _))))
                                      ...
                                      body ...))))
                   ...+))
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
               [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
           (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
         (define whos (map (lambda (who-clause) (extract-who who-clause stx)) (attribute who-clause)))
         (define any-unsafe? (ormap (lambda (kw) kw) (attribute unsafe-kw)))
         (when any-unsafe?
           (check-consistent-unsafes stx (attribute unsafe-kw))
           (for ([args (in-list (syntax->list #'(args ...)))])
             (check-args-for-unsafe stx args #f)))
         (define-values (proc arity)
           (build-case-function no-adjustments '()
                                (add-name-prefix name-prefix the-name) whos (map (lambda (x) #f) whos)
                                #f #f null
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                (attribute ret.converter) (attribute ret.annot-str) (attribute ret.origins)
                                (filter-whos+unsafes #'(rhs ...))
                                stx))
         (define unsafe-proc (and any-unsafe?
                                  (build-unsafe-case-function
                                   #'((arg.kw ...) ...) #'((arg ...) ...) #'((arg.parsed ...) ...)
                                   #'(rest.arg ...) #'(rest.parsed ...)
                                   (attribute unsafe)
                                   stx)))
         (define unsafe-id (and unsafe-proc (car (generate-temporaries (list the-name)))))
         (define extends (car (syntax->list #'(name.extends ...))))
         (maybe-add-unsafe-definition
          unsafe-id unsafe-proc
          (maybe-add-function-result-definition
           the-name extends (syntax->list #'(ret.static-infos ...)) arity unsafe-id
           (build-definitions/maybe-extension #f the-name extends
                                              proc)))]
        ;; both header and alts --- almost the same, but with a declared name and maybe return annotation
        [(form-id main-name-seq::dotted-identifier-sequence main-ret::ret-annotation
                  (~optional (_::block
                              (~alt
                               (~optional (~and reflect-name-clause (group #:name . _)))
                               (~optional (~and who-clause (group #:who . _)))
                               (~optional (group (~and doc-kw #:doc) . doc)))
                              ...))
                  (alts-tag::alts
                   (_::block
                    (group name-seq::dotted-identifier-sequence
                           (~and args-form (_::parens arg::kw-binding ... rest::maybe-arg-rest))
                           (~var ret (:ret-annotation (parse-arg-context #'args-form)))
                           (~and rhs (_::block
                                      (~alt
                                       (~optional (~and inner-who-clause (group #:who . _)))
                                       (~optional (~and unsafe (group (~and unsafe-kw #:unsafe) . _))))
                                      ...
                                      body ...))))
                   ...+))
         #:with main-name::dotted-identifier #'main-name-seq
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name #'main-name.name)
         (check-consistent stx (cons the-name names) "name" #:has-main? #t)
         (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
               [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
           (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
         (define reflect-name (extract-name (attribute reflect-name-clause) stx))
         (define whos (let ([who (extract-who (attribute who-clause) stx)])
                        (for/list ([rhs (in-list (syntax->list #'(rhs ...)))])
                          who)))
         (define inner-whos (map (lambda (who-clause) (extract-who who-clause stx)) (attribute inner-who-clause)))
         (define any-unsafe? (ormap (lambda (kw) kw) (attribute unsafe-kw)))
         (when any-unsafe?
           (check-consistent-unsafes stx (attribute unsafe-kw))
           (for ([args (in-list (syntax->list #'(args-form ...)))])
             (check-args-for-unsafe stx args #f)))
         (define-values (proc arity)
           (build-case-function no-adjustments '()
                                (or reflect-name (add-name-prefix name-prefix the-name)) whos inner-whos
                                (attribute main-ret.converter) (attribute main-ret.annot-str) (attribute main-ret.origins)
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                (attribute ret.converter) (attribute ret.annot-str) (attribute ret.origins)
                                (filter-whos+unsafes #'(rhs ...))
                                stx))
         (define unsafe-proc (and any-unsafe?
                                  (build-unsafe-case-function
                                   #'((arg.kw ...) ...) #'((arg ...) ...) #'((arg.parsed ...) ...)
                                   #'(rest.arg ...) #'(rest.parsed ...)
                                   (attribute unsafe)
                                   stx)))
         (define unsafe-id (and unsafe-proc (car (generate-temporaries (list the-name)))))
         (define extends (car (syntax->list #'(name.extends ...))))
         (maybe-add-unsafe-definition
          unsafe-id unsafe-proc
          (maybe-add-doc
           (attribute doc)
           #'form-id
           #'(main-name-seq.head-id main-name-seq.tail-id ...)
           #'([(~@ . name-seq) args-form (~@ . ret)] ...)
           (attribute doc-kw) stx
           (maybe-add-function-result-definition
            the-name extends (list #'main-ret.static-infos) arity unsafe-id
            (build-definitions/maybe-extension #f the-name extends proc))))]
        ;; single-alternative case
        [(form-id name-seq::dotted-identifier-sequence
                  (~and args-form (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest))
                  (~var ret (:ret-annotation (parse-arg-context #'args-form)))
                  (~and rhs (rhs-tag::block (~alt
                                             (~optional (~and reflect-name-clause (group #:name . _)))
                                             (~optional (~and who-clause (group #:who . _)))
                                             (~optional (group (~and doc-kw #:doc) . doc))
                                             (~optional (~and unsafe (group (~and unsafe-kw #:unsafe) . _))))
                                            ...
                                            body ...)))
         #:with name::dotted-identifier #'name-seq
         (define args (syntax->list #'(arg ...)))
         (define kws (syntax->list #'(arg.kw ...)))
         (define defaults (syntax->list #'(arg.default ...)))
         (check-optional-args stx args kws defaults)
         (check-keyword-args stx args kws)
         (define reflect-name (extract-name (attribute reflect-name-clause) stx))
         (when (attribute unsafe)
           (check-args-for-unsafe stx #'args-form #t))
         (define-values (proc arity)
           (build-function no-adjustments '()
                           (or reflect-name (add-name-prefix name-prefix #'name.name)) #f (extract-who (attribute who-clause) stx)
                           #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                           #'rest.arg #'rest.parsed
                           #'rest.kwarg #'rest.kwparsed
                           (attribute ret.converter) (attribute ret.annot-str) (attribute ret.origins)
                           (if (or (attribute doc) reflect-name (attribute who-clause) (attribute unsafe))
                               #'(rhs-tag body ...)
                               #'rhs)
                           stx))
         (define unsafe-proc (and (attribute unsafe)
                                  (build-unsafe-function
                                   #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...)
                                   #'rest.arg #'rest.parsed
                                   (attribute unsafe)
                                   stx)))
         (define unsafe-id (and unsafe-proc (car (generate-temporaries (list #'name.name)))))
         (maybe-add-unsafe-definition
          unsafe-id unsafe-proc
          (maybe-add-doc
           (attribute doc)
           #'form-id
           #'(name-seq.head-id name-seq.tail-id ...)
           #'([(~@ . name-seq) args-form (~@ . ret)])
           (attribute doc-kw) stx
           (maybe-add-function-result-definition
            #'name.name #'name.extends (list #'ret.static-infos) arity unsafe-id
            (build-definitions/maybe-extension #f #'name.name #'name.extends proc))))]
        ;; definition form didn't match, so try parsing as a `fun` expression:
        [(_ (~or* (~seq (_::parens _ ...) _ ...)
                  (~seq (~optional (_::block . _))
                        (_::alts (_::block (group (_::parens _ ...) . _)) ...+))
                  (~seq _ ... (_::alts . _))))
         (syntax-parse #`(group . #,stx)
           [e::expression
            (let ([e (discard-static-infos #'e.parsed)])
              (list #`(#%expression #,e)))])]))))

(define-entry-point-syntax fun
  (entry-point-transformer
   ;; parse function:
   (lambda (stx adjustments)
     (define-values (term tail) (parse-anonymous-function stx adjustments '()))
     (syntax-parse tail
       [() term]
       [_ (raise-syntax-error #f
                              "unexpected extra terms"
                              tail)]))
   ;; extract shape:
   (lambda (stx)
     (parse-anonymous-function-shape stx))))

(define-immediate-callee-syntax fun
  (immediate-callee-transformer
   ;; parse function:
   (lambda (stx static-infoss op-stx op-mode)
     (define-values (term tail) (parse-anonymous-function stx no-adjustments static-infoss))
     (pack-immediate-callee term tail))))

(define-syntax fun/read-only-property
  (entry-point-transformer
   ;; parse function:
   (lambda (stx adjustments)
     (define-values (term tail) (parse-anonymous-function stx adjustments '()))
     term)
   ;; extract shape:
   (lambda (stx)
     (hash 'arity 1))))

(define-for-syntax (parse-anonymous-function stx
                                             [adjustments no-adjustments]
                                             [argument-static-infos '()])
  (syntax-parse stx
    #:datum-literals (group)
    ;; alts case, with maybe a declared return annotation
    [(form-id main-ret::ret-annotation
              (~optional (_::block
                          (~alt (~optional (~and outer-who-clause (group #:who . _)))
                                (~optional (~and reflect-name-clause (group #:name . _))))
                          ...))
              (alts-tag::alts
               (_::block
                (group (~and args-form (_::parens arg::kw-binding ... rest::maybe-arg-rest))
                       (~var ret (:ret-annotation (parse-arg-context #'args-form)))
                       (~and rhs (_::block
                                  (~optional (~and who-clause (group #:who . _)))
                                  body ...))))
               ...+))
     (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
           [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
       (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
     (define falses (map (lambda (s) #f) (syntax->list #'(rhs ...))))
     (define outer-who (extract-who (attribute outer-who-clause) stx))
     (define whos (map (lambda (who-clause) (extract-who who-clause stx)) (attribute who-clause)))
     (define outer-whos (map (lambda (who) outer-who) whos))
     (define reflect-name (extract-name (attribute reflect-name-clause) stx))
     (define-values (proc arity)
       (build-case-function adjustments argument-static-infos
                            (or reflect-name (get-local-name #'form-id adjustments)) outer-whos whos
                            (attribute main-ret.converter) (attribute main-ret.annot-str) (attribute main-ret.origins)
                            #'((arg.kw ...) ...)
                            #'((arg ...) ...) #'((arg.parsed ...) ...)
                            #'(rest.arg ...) #'(rest.parsed ...)
                            #'(rest.kwarg ...) #'(rest.kwparsed ...)
                            (attribute ret.converter) (attribute ret.annot-str) (attribute ret.origins)
                            (filter-whos #'(rhs ...))
                            stx))
     (values (wrap-function-static-info
              (if arity
                  (wrap-static-info proc #'#%function-arity arity)
                  proc))
             #'())]
    ;; single-alternative case
    [(form-id (~and args-form (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest))
              (~var ret (:ret-annotation (parse-arg-context #'args-form)))
              (~and rhs (rhs-tag::block
                         (~alt (~optional (~and reflect-name-clause (group #:name . _)))
                               (~optional (~and who-clause (group #:who . _))))
                         ...
                         body ...)))
     (define args (syntax->list #'(arg ...)))
     (define kws (syntax->list #'(arg.kw ...)))
     (define defaults (syntax->list #'(arg.default ...)))
     (check-optional-args stx args kws defaults)
     (check-keyword-args stx args kws)
     (define reflect-name (extract-name (attribute reflect-name-clause) stx))
     (define-values (fun arity)
       (build-function adjustments argument-static-infos
                       (or reflect-name (get-local-name #'form-id adjustments)) #f (extract-who (attribute who-clause) stx)
                       #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                       #'rest.arg #'rest.parsed
                       #'rest.kwarg #'rest.kwparsed
                       (attribute ret.converter) (attribute ret.annot-str) (attribute ret.origins)
                       (if (or reflect-name (attribute who-clause))
                           #'(rhs-tag body ...)
                           #'rhs)
                       stx))
     (values (let* ([fun (if (pair? (syntax-e #'ret.static-infos))
                             (wrap-static-info fun #'#%call-result #'ret.static-infos)
                             fun)]
                    [fun (if arity
                             (wrap-static-info fun #'#%function-arity arity)
                             fun)])
               (wrap-function-static-info fun))
             #'())]))

(define-for-syntax (filter-whos rhss-stx)
  (datum->syntax
   #f
   (for/list ([rhs (in-list (syntax->list rhss-stx))])
     (syntax-parse rhs
       #:datum-literals (group)
       [(tag::block (group #:who . _) body ...)
        #'(tag body ...)]
       [_ rhs]))))

(define-for-syntax (filter-whos+unsafes rhss-stx)
  (datum->syntax
   #f
   (for/list ([rhs (in-list (syntax->list rhss-stx))])
     (syntax-parse rhs
       #:datum-literals (group)
       [(tag::block (~or (group #:who . _) (group #:unsafe . _)) ...+ body ...)
        #'(tag body ...)]
       [_ rhs]))))

(define-for-syntax (maybe-add-doc doc form-id names headers doc-kw-stx orig-stx defns)
  (cond
    [(and doc
          (not (eq? 'top-level (syntax-local-context))))
     (define id (build-dot-symbol (syntax->list names)))
     (define gs
       (syntax-parse doc
         [((tag::block g ...)) #'(g ...)]
         [() #'()]
         [_ (raise-syntax-error #f
                                "expected nothing or a block after `~doc`"
                                orig-stx
                                doc)]))
     (define (add-form header)
       #`(group #,form-id #,@header))
     (define (replace-:~-ret header)
       (syntax-parse header
         #:datum-literals (parens op :~)
         [(pre ... (~and args (parens _ ...))
               ((~and op-id op) (~and colon-tilde :~))
               post ...)
          (datum->syntax header
                         (append (syntax->list #'(pre ...))
                                 (list #'args)
                                 (list (datum->syntax #f
                                                      (list #'op-id
                                                            (syntax-raw-property
                                                             (datum->syntax #'colon-tilde
                                                                            '::
                                                                            #'colon-tilde
                                                                            #'colon-tilde)
                                                             "::"))))
                                 (syntax->list #'(post ...))))]
         [_ header]))
     (cons
      #`(rhombus-module+
         #,(datum->syntax #f 'doc doc-kw-stx)
         #:orig #,orig-stx
         #:language (lib "rhombus/doc.rhm")
         (group export #,id)
         (group def #,id
                (op =)
                DocSpec
                (parens (group List
                               (parens
                                #,@(for/list ([header (in-list (syntax->list headers))])
                                     #`(group Syntax (op |.|) literal (quotes #,(replace-:~-ret
                                                                                 (strip-context (add-form header))))))))
                        (group List
                               (parens
                                #,@(for/list ([g (in-list (syntax->list gs))])
                                     #`(group Syntax (op |.|) literal (quotes #,g))))))))
      defns)]
    [else defns]))

(define-syntax Function.pass/optimize
  (expression-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id (~and p (tag::parens arg-g ...)) . new-tail)
        #:when (simple-call? tail #:ellipsis-ok? #t)
        (define es
          (let loop ([gs-stx #'(arg-g ...)])
            (syntax-parse gs-stx
              #:datum-literals (group)
              [() '()]
              [(rep-arg (group _::...-expr) . gs)
               (define-values (new-gs extras) (consume-extra-ellipses #'gs))
               (define e (syntax-parse #'rep-arg
                           [rep::repetition
                            (render-repetition #'for/single-valued #'rep.parsed
                                               #:depth (add1 extras))]))
               (cons e (loop new-gs))]
              [(g . gs)
               (define e (syntax-parse #'g
                           [e::expression (discard-static-infos #'e.parsed)]))
               (cons e (loop #'gs))])))
        (values
         (relocate+reraw
          (respan (datum->syntax #f (list #'form-id #'p)))
          #`(void #,@es))
         #'new-tail)]
       [(form-id . new-tail)
        (values
         (relocate-id #'form-id #'Function.pass)
         #'new-tail)]))))

(define/arity (Function.black_box v)
  (meta-if-version-at-least
   "8.18.0.17"
   (black-box v)
   v))

(define-syntax (for/single-valued stx)
  (syntax-parse stx
    [(_ clauses body)
     #'(for clauses (void body))]))

(define-repetition-syntax Function.pass/optimize
  (repetition-transformer
   (lambda (tail)
     (syntax-parse tail
       [(form-id . new-tail)
        (values
         (identifier-repetition-use #'Function.pass)
         #'new-tail)]))))

(define Function.pass
  (make-keyword-procedure
   (lambda (kws kw-args . args)
     (void))
   (let ([Function.pass (lambda args
                          (void))])
     Function.pass)))

(define-static-info-syntax Function.pass
  (#%function-arity (-1 () #f))
  . #,(get-function-static-infos))

(define/method (Function.name p)
  #:static-infos ((#%call-result ((#%maybe #,(get-symbol-static-infos)))))
  (check-proc who p)
  (object-name p))

(define/method (Function.realm p)
  #:primitive (procedure-realm)
  #:static-infos ((#%call-result #,(get-symbol-static-infos)))
  (procedure-realm p))

(define/method (Function.rename p name #:realm [realm rhombus-realm])
  #:primitive (procedure-rename)
  #:static-infos ((#%call-result #,(get-function-static-infos)))
  (procedure-rename p name realm))

(define/method (Function.arity p)
  #:static-infos ((#%call-result ((#%values (#,(get-int-static-infos)
                                             #,(get-treelist-static-infos)
                                             ((#%maybe #,(get-treelist-static-infos))))))))
  (check-proc who p)
  (define-values (req-kws allow-kws) (procedure-keywords p))
  (values (procedure-arity-mask p)
          (list->treelist req-kws)
          (and allow-kws (list->treelist allow-kws))))

(define/method (Function.reduce_arity p
                                      mask
                                      req-kws
                                      allow-kws
                                      #:name [name (object-name p)]
                                      #:realm [realm rhombus-realm])
  #:primitive (procedure-reduce-keyword-arity-mask procedure-reduce-arity-mask)
  #:static-infos ((#%call-result #,(get-function-static-infos)))
  (check-proc who p)
  (unless (exact-integer? mask) (raise-annotation-failure who mask "Int"))
  (unless (and (treelist? req-kws) (for/and ([kw (in-treelist req-kws)]) (keyword? kw)))
    (raise-annotation-failure who req-kws "List.of(Keyword)"))
  (unless (or (not allow-kws) (and (treelist? allow-kws) (for/and ([kw (in-treelist allow-kws)]) (keyword? kw))))
    (raise-annotation-failure who allow-kws "maybe(List.of(Keyword))"))
  (procedure-reduce-keyword-arity-mask p
                                       mask
                                       (treelist->list req-kws)
                                       (and allow-kws (treelist->list allow-kws))
                                       name
                                       realm))

(begin-for-syntax
  (install-get-function-static-infos! get-function-static-infos))
