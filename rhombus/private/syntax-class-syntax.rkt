#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "tag.rkt")
         syntax/parse/pre
         (submod "quasiquote.rkt" convert)
         (submod "syntax-class.rkt" for-quasiquote)
         (submod "syntax-class.rkt" for-syntax-class-syntax)
         "syntax-class-clause.rkt"
         (submod "syntax-class-clause-primitive.rkt" for-class)
         "syntax-class-arg.rkt"
         "pattern-clause.rkt"
         "pattern-variable.rkt"
         "definition.rkt"
         "name-root.rkt"
         "parse.rkt"
         "parsed.rkt"
         "pack.rkt"
         "parens.rkt"
         (only-in "def+let.rkt" def)
         (rename-in "ellipsis.rkt"
                    [... rhombus...]))

(provide (rename-out [rhombus-syntax syntax]))

(define-simple-name-root rhombus-syntax
  class
  only)

(define-name-root only
  #:fields
  [class only-class])

(define-for-syntax (make-class-definer define-class-id)
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        ;; immediate-patterns shorthand
        [(form-id class-name args::class-args (_::alts alt ...))
         (generate-syntax-class stx define-class-id #'class-name #'args.formals #'args.arity
                                '#:sequence (syntax->list #'(alt ...)) #f #f #f #f)]
        ;; Patterns within `matching`
        [(form-id class-name args::class-args
                  ;; syntax-class clauses are impleemnted in "syntax-class-clause-primitive.rkt"
                  (_::block clause::syntax-class-clause ...))
         (define-values (pattern-alts kind-kw class-desc fields-ht opaque?)
           (extract-clauses stx (syntax->list #'(clause.parsed ...))))
         (generate-syntax-class stx define-class-id #'class-name #'args.formals #'args.arity
                                kind-kw pattern-alts class-desc fields-ht opaque? #f)]))))

(begin-for-syntax
  (struct definition+syntax-class-parser (def pars)
    #:property prop:definition-transformer (lambda (self) (definition+syntax-class-parser-def self))
    #:property prop:syntax-class-parser (lambda (self) (definition+syntax-class-parser-pars self))))

(define-syntax class (definition+syntax-class-parser
                       (make-class-definer #'define-syntax)
                       (syntax-class-parser
                        (lambda (who stx expected-kind name tail)
                          (parse-anonymous-syntax-class who stx expected-kind name tail)))))
(define-syntax only-class (make-class-definer #'define-syntax-class-syntax))

(define-for-syntax (parse-anonymous-syntax-class who orig-stx expected-kind name tail)
  (syntax-parse tail
    ;; immediate-patterns shorthand
    [((_::alts alt ...))
     (generate-syntax-class orig-stx #f name #'#f #'#f
                            '#:sequence (syntax->list #'(alt ...)) #f #f #f
                            expected-kind)]
    ;; patterns within `matching`
    [((_::block clause::syntax-class-clause ...))
     (define-values (pattern-alts kind-kw class-desc fields-ht opaque?)
       (extract-clauses orig-stx (syntax->list #'(clause.parsed ...))))
     (generate-syntax-class orig-stx #f name #'#f #'#f
                            kind-kw pattern-alts class-desc fields-ht opaque? expected-kind)]
    [_ (raise-syntax-error who "bad syntax" orig-stx)]))

;; if `define-syntax-id` is #f, returns a `rhombus-syntax-class` value directly,
;; and `class/inline-name` is actually a name to extend for attribute names
(define-for-syntax (generate-syntax-class stx define-syntax-id class/inline-name class-formals class-arity
                                          kind-kw alts description-expr fields-ht opaque? expected-kind)
  (define-values (kind splicing?)
    (let ([kind (string->symbol (keyword->string kind-kw))])
      (cond
        [(eq? kind 'sequence) (values 'term #t)]
        [else (values kind #f)])))
  (cond
    [(and (or (eq? expected-kind 'block)
              (eq? expected-kind 'multi)
              (eq? expected-kind 'group))
          (eq? kind 'term))
     ;; shortcut to avoid redundant parsing when it's not going to work out
     #f]
    [else
     (define-values (patterns attributes)
       (for/lists (patterns attributess
                            #:result (values patterns (intersect-attributes stx attributess fields-ht)))
           ([alt-stx (in-list alts)])
         (generate-pattern-and-attributes stx alt-stx kind splicing?)))
     (cond
       [(not define-syntax-id)
        (rhombus-syntax-class kind
                              (syntax-class-body->inline patterns class/inline-name)
                              (datum->syntax #f (map pattern-variable->list attributes))
                              splicing?
                              (syntax->datum class-arity))]
       [else
        (define class-name class/inline-name)
        (define define-class (if splicing?
                                 #'define-splicing-syntax-class
                                 #'define-syntax-class))
        (list
         #`(#,define-class #,(if (syntax-e class-formals)
                                 #`(#,class-name . #,class-formals)
                                 class-name)
            #:description #,(or description-expr #f)
            #:datum-literals (block group quotes)
            #:attributes #,(for/list ([var attributes])
                             #`[#,(pattern-variable-sym var) #,(pattern-variable-depth var)])
            #,@(if opaque? '(#:opaque) '())
            #,@patterns)
         #`(#,define-syntax-id #,(in-syntax-class-space class-name)
            (rhombus-syntax-class '#,kind
                                  #'#,class-name
                                  (quote-syntax #,(map pattern-variable->list attributes))
                                  #,splicing?
                                  '#,class-arity)))])]))

;; ----------------------------------------

(begin-for-syntax
  (define-syntax-class :attribute-lhs
    #:datum-literals (brackets group op)
    #:literals (rhombus...)
    (pattern id:identifier
             #:attr depth #'0)
    (pattern (brackets (group a::attribute-lhs) (group (op rhombus...)))
             #:attr id #'a.id
             #:attr depth #`#,(+ 1 (syntax-e #'a.depth)))))

(define-for-syntax (generate-pattern-and-attributes orig-stx stx kind splicing?)
  (define-values (pat body)
    (syntax-parse stx
      #:datum-literals (group)
      [(block (group (~and pat (_::quotes . _))))
       (values #'pat #'())]
      [(block (group (~and pat (_::quotes . _))
                     (_::block body ...)))
       (values #'pat #'(body ...))]))
  
  (define in-quotes
    (cond
      [(eq? kind 'multi)
       (syntax-parse pat
         [(_ g ...) #'(multi g ...)])]
      [(eq? kind 'block)
       (syntax-parse pat
         #:datum-literals (block)
         [(_ (~and g (group (block . _)))) #'g]
         [_ (raise-syntax-error #f
                                "not a block pattern"
                                orig-stx
                                pat)])]
      [else
       (syntax-parse pat
         [(_ g) #'g]
         [(_)
          (if (and (eq? kind 'term) splicing?)
              #'(group)
              (raise-syntax-error #f
                                  "no groups in pattern"
                                  orig-stx
                                  pat))]
         [_ (raise-syntax-error #f
                                "multiple groups in pattern"
                                orig-stx
                                pat)])]))
  
  (define-values (p idrs sidrs vars can-be-empty?)
    (convert-pattern #:splice? (not (eq? kind 'group))
                     #:splice-pattern (and (not (eq? kind 'group))
                                           (not splicing?)
                                           (lambda (ps)
                                             (cond
                                               [(eq? kind 'multi)
                                                #`(_ #,@ps)]
                                               [(= 1 (length ps))
                                                (car ps)]
                                               [else
                                                (raise-syntax-error #f
                                                                    "not a single-term pattern"
                                                                    orig-stx
                                                                    pat)])))
                     in-quotes))

  (define (bindings->defns idrs sidrs)
    (with-syntax ([([val-id val-rhs] ...) idrs]
                  [([stx-id stx-rhs] ...) sidrs])
      #'[(define val-id val-rhs)
         ...
         (define-syntax stx-id stx-rhs)
         ...]))
  
  (define-values (pattern-body all-attrs)
    (let loop ([body (syntax->list body)]
               [rev-do null]
               [rev-body (list (bindings->defns idrs sidrs) '#:do)]
               [rev-attrs (reverse vars)])
      (define (accum-do) (if (null? rev-do)
                             rev-body
                             (list* #`[(rhombus-body-sequence #,@(reverse rev-do))] '#:do
                                    rev-body)))
      (cond
        [(null? body) (values (reverse (accum-do))
                              (reverse rev-attrs))]
        [else
         (define g (car body))
         (cond
           [(pattern-clause? g)
            (syntax-parse g
              [c::pattern-clause
               (syntax-parse #'c.parsed
                 [(#:field id depth rhs)
                  #:with (tmp-id) (generate-temporaries #'(id))
                  (loop (cdr body)
                        null
                        (list* #'[(define tmp-id rhs)
                                  (define-syntax id
                                    (make-pattern-variable-syntax (quote-syntax id)
                                                                  (quote-syntax tmp-id)
                                                                  (quote-syntax unpack-element*)
                                                                  depth
                                                                  #f
                                                                  #'()))] '#:do
                               (accum-do))
                        (cons (pattern-variable (syntax-e #'id) #'tmp-id (syntax-e #'depth) (quote-syntax unpack-element*))
                              rev-attrs))]
                 [(#:also (_ pat-g ...) rhs)
                  (define-values (p idrs sidrs vars can-be-empty?)
                    (convert-pattern #'(multi pat-g ...)))
                  (loop (cdr body)
                        null
                        (list* (bindings->defns idrs sidrs) '#:do
                               #' (repack-as-multi rhs) p '#:with
                               (accum-do))
                        (append (reverse vars)
                                rev-attrs))]
                 [(#:when rhs)
                  (loop (cdr body)
                        null
                        (list* #'rhs '#:when
                               (accum-do))
                        rev-attrs)])])]
           [else
            (loop (cdr body) (cons g rev-do) rev-body rev-attrs)])])))
  
  (with-syntax ([((attr ...) ...)
                 (map (lambda (var)
                        #`(#:attr
                           (#,(pattern-variable-sym var) #,(pattern-variable-depth var))
                           (#,(pattern-variable-unpack*-id var)
                            (quote-syntax dots)
                            #,(pattern-variable-val-id var)
                            #,(pattern-variable-depth var))))
                      all-attrs)]
                [(body-form ...) pattern-body])
    (values #`(pattern #,p
                       #,@pattern-body
                       attr ... ...)
            all-attrs)))

;; converts a `pattern` clause for `syntax-case` into a pattern suitable
;; for directly inlinding into a larger pattern
(define-for-syntax (syntax-class-body->inline patterns inline-name)
  #`(~or #,@(for/list ([pattern (in-list patterns)])
              (syntax-parse pattern
                [(_ pat body ...)
                 #`(~and pat
                         #,@(let loop ([body (syntax->list #'(body ...))])
                              (cond
                                [(null? body) '()]
                                [(eq? (syntax-e (car body)) '#:do)
                                 (cons #`(~do . #,(cadr body))
                                       (loop (cddr body)))]
                                [(eq? (syntax-e (car body)) '#:with)
                                 (cons #`(~parse #,(cadr body) #,(caddr body))
                                       (loop (cdddr body)))]
                                [(eq? (syntax-e (car body)) '#:when)
                                 (cons #`(~fail #:unless #,(cadr body))
                                       (loop (cddr body)))]
                                [(eq? (syntax-e (car body)) '#:attr)
                                 (with-syntax ([[name depth] (cadr body)])
                                   (define id.name (datum->syntax inline-name
                                                                  (string->symbol (format "~a.~a"
                                                                                          (syntax-e inline-name)
                                                                                          (syntax-e #'name)))
                                                                  #'name))
                                   (cons #`(~bind ([#,id.name depth] #,(caddr body)))
                                         (loop (cdddr body))))]
                                [else (error "unhandled pattern body" body)])))]))))

;; ----------------------------------------

(define-for-syntax (intersect-attributes stx attributess fields-ht)
  (cond
    [(and (null? attributess) (not fields-ht)) '()]
    [(and (null? (cdr attributess)) (not fields-ht)) (car attributess)]
    [else
     ;; start with initial set
     (define ht0
       (for/hasheq ([var (in-list (car attributess))])
         (values (pattern-variable-sym var) var)))
     ;; intersect by pruning set
     (define ht
       (for/fold ([ht0 ht0]) ([attributes (in-list (cdr attributess))])
         (for/fold ([ht #hasheq()]) ([var (in-list attributes)])
           (define prev-var (hash-ref ht0 (pattern-variable-sym var) #f))
           (if prev-var
               (hash-set ht
                         (pattern-variable-sym var)
                         (intersect-var stx var prev-var))
               ht))))
     ;; filter by declared fields
     (define filtered-ht
       (if fields-ht
           (for/hasheq ([k (in-hash-keys fields-ht)])
             (define var (hash-ref ht k #f))
             (unless var
               (raise-syntax-error #f
                                   "field not available from all pattern cases"
                                   stx
                                   (hash-ref fields-ht k)))
             (values k var))
           ht))
     ;; convert back to list
     (hash-values filtered-ht #t)]))

(define-for-syntax (intersect-var stx a b)
  (unless (eqv? (pattern-variable-depth a) (pattern-variable-depth b))
    (raise-syntax-error #f
                        "field with different depths in different cases"
                        stx
                        (pattern-variable-sym a)))
  ;; keeping the same unpack, if possible, enables optimizations for
  ;; tail repetitions; otherwise, the term is sufficiently normalized
  ;; by matching that we can just use `unpack-element*`
  (if (free-identifier=? (pattern-variable-unpack*-id a) (pattern-variable-unpack*-id b))
      a
      (struct-copy pattern-variable a
                   [unpack*-id #'unpack-element*])))
