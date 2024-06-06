#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "consistent.rkt"
                     "entry-point-adjustment.rkt")
         racket/keyword
         "treelist.rkt"
         "provide.rkt"
         (submod "function-parse.rkt" for-build)
         (submod "list.rkt" for-compound-repetition)
         (only-in "implicit.rkt" #%literal)
         "parens.rkt"
         "expression.rkt"
         "definition.rkt"
         "entry-point.rkt"
         "immediate-callee.rkt"
         "parse.rkt"
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         "dotted-sequence-parse.rkt"
         "realm.rkt"
         "define-arity.rkt"
         (submod "define-arity.rkt" for-info)
         "class-primitive.rkt"
         "rhombus-primitive.rkt")

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

(define-primitive-class Function function
  #:lift-declaration
  #:no-constructor-static-info
  #:existing
  #:opaque
  #:fields ()
  #:namespace-fields
  (of_arity
   pass)
  #:properties
  ()
  #:methods
  (map
   for_each))

(set-primitive-who! 'application '|function call|)

(set-primitive-contract-combinator!
 'procedure-arity-includes/c
 (lambda (form)
   (and (pair? (cdr form))
        (exact-nonnegative-integer? (cadr form))
        (null? (cddr form))
        (string-append "Function.of_arity(" (number->string (cadr form)) ")"))))

(define (check-proc who proc)
  (unless (procedure? proc)
    (raise-argument-error* who rhombus-realm "Function" proc)))

(define (check-list who l)
  (unless (treelist? l)
    (raise-argument-error* who rhombus-realm "List" l)))

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
  #:static-infos ((#%call-result #,(get-treelist-static-infos))))
(define-map for_each for)

(define-for-syntax (wrap-function-static-info expr)
  (wrap-static-info* expr (get-function-static-infos)))

(define-annotation-syntax Function (identifier-annotation procedure? #,(get-function-static-infos)))

(define-annotation-syntax of_arity
  (annotation-prefix-operator
   '((default . stronger))
   'macro
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens g ...+) . tail)
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
                                                        #:when (free-identifier=? #'#%literal (datum->syntax #'n '#%literal))
                                                        #`((#%function-arity (#,(arithmetic-shift 1 (syntax-e #'n))
                                                                              ()
                                                                              (kw ...))))]
                                                       [_ #'()])])
            (values (annotation-predicate-form
                     #'(let ([n (rhombus-expression g)]
                             ...)
                         (check-nonneg-int 'Function.of_arity n)
                         ...
                         (lambda (v)
                           (and (procedure? v)
                                (procedure-arity-includes? v n kw-ok?)
                                ...
                                kw-check)))
                     #`(function-arity-static ... . #,(get-function-static-infos)))
                    #'tail)))]))))

(define (check-nonneg-int who v)
  (unless (exact-nonnegative-integer? v)
    (raise-argument-error* who rhombus-realm "NonnegInt" v)))

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
  (define (get-local-name who)
    (or (syntax-local-name) who))

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
    (void)))

(define-syntax fun
  (expression-transformer
   (lambda (stx)
     (parse-anonymous-function stx no-adjustments '()))))

(define-defn-syntax fun
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        ;; immediate alts case
        [(form-id (alts-tag::alts
                   (_::block
                    (group name-seq::dotted-identifier-sequence (_::parens arg::kw-binding ... rest::maybe-arg-rest)
                           ret::ret-annotation
                           (~and rhs (_::block body ...))))
                   ...+))
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
               [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
           (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
         (define-values (proc arity)
           (build-case-function no-adjustments '()
                                the-name
                                #f #f
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                (attribute ret.converter) (attribute ret.annot-str)
                                #'(rhs ...)
                                stx))
         (maybe-add-function-result-definition
          the-name (syntax->list #'(ret.static-infos ...)) arity
          (build-definitions/maybe-extension #f the-name (car (syntax->list #'(name.extends ...)))
                                             proc))]
        ;; both header and alts --- almost the same, but with a declared name and maybe return annotation
        [(form-id main-name-seq::dotted-identifier-sequence main-ret::ret-annotation
                  (alts-tag::alts
                   (_::block
                    (group name-seq::dotted-identifier-sequence (_::parens arg::kw-binding ... rest::maybe-arg-rest)
                           ret::ret-annotation
                           (~and rhs (_::block body ...))))
                   ...+))
         #:with main-name::dotted-identifier #'main-name-seq
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name #'main-name.name)
         (check-consistent stx (cons the-name names) "name" #:has-main? #t)
         (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
               [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
           (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
         (define-values (proc arity)
           (build-case-function no-adjustments '()
                                the-name
                                (attribute main-ret.converter) (attribute main-ret.annot-str)
                                #'((arg.kw ...) ...)
                                #'((arg ...) ...) #'((arg.parsed ...) ...)
                                #'(rest.arg ...) #'(rest.parsed ...)
                                #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                (attribute ret.converter) (attribute ret.annot-str)
                                #'(rhs ...)
                                stx))
         (maybe-add-function-result-definition
          the-name (list #'main-ret.static-infos) arity
          (build-definitions/maybe-extension #f the-name (car (syntax->list #'(name.extends ...)))
                                             proc))]
        ;; single-alterative case
        [(form-id name-seq::dotted-identifier-sequence (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest)
                  ret::ret-annotation
                  (~and rhs (_::block body ...)))
         #:with name::dotted-identifier #'name-seq
         (define args (syntax->list #'(arg ...)))
         (define kws (syntax->list #'(arg.kw ...)))
         (define defaults (syntax->list #'(arg.default ...)))
         (check-optional-args stx args kws defaults)
         (check-keyword-args stx args kws)
         (define-values (proc arity)
           (build-function no-adjustments '()
                           #'name.name
                           #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                           #'rest.arg #'rest.parsed
                           #'rest.kwarg #'rest.kwparsed
                           (attribute ret.converter) (attribute ret.annot-str)
                           #'rhs
                           stx))
         (maybe-add-function-result-definition
          #'name.name (list #'ret.static-infos) arity
          (build-definitions/maybe-extension #f #'name.name #'name.extends
                                             proc))]
        ;; definition form didn't match, so try parsing as a `fun` expression:
        [(_ (~or* (~seq (_::parens _ ...) _ ...)
                  (_::alts (_::block (group (_::parens _ ...) . _)) ...+)
                  (~seq _ ... (_::alts . _))))
         (syntax-parse #`(group . #,stx)
           [e::expression
            (list #'(#%expression e.parsed))])]))))

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
   ;; extract arity:
   (lambda (stx)
     (parse-anonymous-function-arity stx))))

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
   ;; extract arity:
   (lambda (stx)
     1)))

(define-for-syntax (parse-anonymous-function stx
                                             [adjustments no-adjustments]
                                             [argument-static-infos '()])
  (syntax-parse stx
    #:datum-literals (group)
    ;; alts case, with maybe a declared return annotation
    [(form-id main-ret::ret-annotation
              (alts-tag::alts
               (_::block
                (group (_::parens arg::kw-binding ... rest::maybe-arg-rest) ret::ret-annotation
                       (~and rhs (_::block body ...))))
               ...+))
     (for ([args-stx (in-list (syntax->list #'((arg ...) ...)))]
           [kws-stx (in-list (syntax->list #'((arg.kw ...) ...)))])
       (check-keyword-args stx (syntax->list args-stx) (syntax->list kws-stx)))
     (define-values (proc arity)
       (build-case-function adjustments argument-static-infos
                            (get-local-name #'form-id)
                            (attribute main-ret.converter) (attribute main-ret.annot-str)
                            #'((arg.kw ...) ...)
                            #'((arg ...) ...) #'((arg.parsed ...) ...)
                            #'(rest.arg ...) #'(rest.parsed ...)
                            #'(rest.kwarg ...) #'(rest.kwparsed ...)
                            (attribute ret.converter) (attribute ret.annot-str)
                            #'(rhs ...)
                            stx))
     (values (wrap-function-static-info
              (if arity
                  (wrap-static-info proc #'#%function-arity arity)
                  proc))
             #'())]
    ;; single-alterative case
    [(form-id (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest) ret::ret-annotation
              (~and rhs (_::block body ...)))
     (define args (syntax->list #'(arg ...)))
     (define kws (syntax->list #'(arg.kw ...)))
     (define defaults (syntax->list #'(arg.default ...)))
     (check-optional-args stx args kws defaults)
     (check-keyword-args stx args kws)
     (define-values (fun arity)
       (build-function adjustments argument-static-infos
                       (get-local-name #'form-id)
                       #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                       #'rest.arg #'rest.parsed
                       #'rest.kwarg #'rest.kwparsed
                       (attribute ret.converter) (attribute ret.annot-str)
                       #'rhs
                       stx))
     (values (let* ([fun (if (pair? (syntax-e #'ret.static-infos))
                             (wrap-static-info fun #'#%call-result #'ret.static-infos)
                             fun)]
                    [fun (if arity
                             (wrap-static-info fun #'#%function-arity arity)
                             fun)])
               (wrap-function-static-info fun))
             #'())]))

(define pass
  (make-keyword-procedure
   (let ([pass (lambda (kws kw-args . args)
                 (void))])
     pass)))

(begin-for-syntax
  (install-get-function-static-infos! get-function-static-infos))
