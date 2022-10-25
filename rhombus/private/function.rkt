#lang racket/base
(require (for-syntax racket/base
                     (only-in racket/function normalize-arity)
                     (only-in racket/set set-intersect set-union)
                     racket/syntax
                     syntax/parse
                     "srcloc.rkt"
                     "consistent.rkt"
                     "with-syntax.rkt"
                     "tag.rkt")
         racket/unsafe/undefined
         "parens.rkt"
         "expression.rkt"
         "binding.rkt"
         "definition.rkt"
         "expression+definition.rkt"
         "parse.rkt"
         "nested-bindings.rkt"
         "name-root.rkt"
         "call-result-key.rkt"
         "ref-result-key.rkt"
         "static-info.rkt"
         "annotation.rkt"
         (only-in "ellipsis.rkt"
                  [... rhombus...])
         "repetition.rkt"
         "rest-marker.rkt"
         (only-in "list.rkt" List)
         (only-in (submod "list.rkt" for-binding)
                  parse-list-expression)
         (submod "annotation.rkt" for-class)
         (only-in "equal.rkt"
                  [= rhombus=])
         "dotted-sequence-parse.rkt"
         "lambda-kwrest.rkt"
         "error.rkt"
         (submod "dot.rkt" for-dot-provider)
         "dot-parse.rkt"
         "realm.rkt")

(provide fun
         Function
         (for-space rhombus/annotation Function))

(module+ for-build
  (provide (for-syntax :kw-binding
                       :kw-opt-binding
                       :ret-annotation
                       :maybe-arg-rest
                       :non-...-binding
                       build-function
                       build-case-function
                       maybe-add-function-result-definition)))

(module+ for-call
  (provide (for-syntax parse-function-call)))

(module+ for-builtin
  (provide function-method-table))

(define-name-root Function
  #:fields
  (map))

(define function-method-table
  (hash 'map (lambda (f) (lambda lists (apply map f lists)))))

(define-for-syntax function-static-infos
  #'((#%dot-provider function-instance)))

(define-for-syntax (wrap-function-static-info expr)
  (wrap-static-info* expr function-static-infos))

(define-annotation-syntax Function (identifier-annotation #'Function #'procedure? #'()))

(define-syntax function-instance
  (dot-provider-more-static
   (dot-parse-dispatch
    (lambda (field-sym field ary 0ary nary fail-k)
      (case field-sym
        [(map) (nary -2 #'map)]
        [else #f])))))

(begin-for-syntax
  (define-syntax-class :non-...-binding
    (pattern form
             #:when (syntax-parse #'form
                      #:datum-literals (group op)
                      #:literals (& ~&)
                      [(group (op (~literal rhombus...))) #f]
                      [(group (op (~or & ~&)) . _) #f]
                      [_ #t])
             #:with arg::binding #'form
             #:with parsed #'arg.parsed))
  
  (define (empty->keyword g kw)
    (syntax-parse g
      [(_) #`(group #,(datum->syntax kw (string->symbol (keyword->string (syntax-e kw))) kw))]
      [_ g]))

  (define-syntax-class :kw-binding
    #:attributes [kw parsed]
    #:datum-literals (op block group)
    #:literals (rhombus= rhombus...)
    (pattern (group kw:keyword (block (group a ...)))
             #:with arg::binding (empty->keyword #'(group a ...) #'kw)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword)
             #:with arg::binding (empty->keyword #'(group) #'kw)
             #:attr parsed #'arg.parsed)
    (pattern arg::non-...-binding
             #:attr kw #'#f
             #:attr parsed #'arg.parsed))
  
  (define-syntax-class :kw-opt-binding
    #:attributes [kw parsed default]
    #:datum-literals (op block group)
    #:literals (rhombus= rhombus...)
    (pattern (group kw:keyword (block (group a ... (op rhombus=) e ...+)))
             #:with arg::binding (empty->keyword #'(group a ...) #'kw)
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (block (group (op rhombus=) e ...+)))
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group kw:keyword (op rhombus=) e ...+)
             #:with arg::binding (empty->keyword #'(group) #'kw)
             #:with default #'(group e ...)
             #:attr parsed #'arg.parsed)
    (pattern (group a ...+ (op rhombus=) e ...+)
             #:with arg::binding #'(group a ...)
             #:with default #'(group e ...)
             #:attr kw #'#f
             #:attr parsed #'arg.parsed)
    (pattern ::kw-binding
             #:attr default #'#f))

  (define-syntax-class :not-block
    #:datum-literals (op parens braces)
    (pattern _:identifier)
    (pattern (op . _))
    (pattern (parens . _))
    (pattern (braces . _)))

  (define-splicing-syntax-class :ret-annotation
    #:datum-literals (block group op)
    #:literals (:: -:)
    (pattern (~seq (op ::) ctc0::not-block ctc::not-block ...)
             #:with c::annotation (no-srcloc #`(#,group-tag ctc0 ctc ...))
             #:with c-parsed::annotation-form #'c.parsed
             #:attr static-infos #'c-parsed.static-infos
             #:attr predicate #'c-parsed.predicate)
    (pattern (~seq (op -:) ctc0::not-block ctc::not-block ...)
             #:with c::annotation (no-srcloc #`(#,group-tag ctc0 ctc ...))
             #:with c-parsed::annotation-form #'c.parsed
             #:attr static-infos #'c-parsed.static-infos
             #:attr predicate #'#f)
    (pattern (~seq)
             #:attr static-infos #'()
             #:attr predicate #'#f))

  (define-splicing-syntax-class :pos-rest
    #:attributes [arg parsed]
    #:datum-literals (group op)
    #:literals (& rhombus...)
    (pattern (~seq (group (op &) a ...))
      #:with arg::non-...-binding #'(group a ...)
      #:with parsed #'arg.parsed)
    (pattern (~seq e::non-...-binding (~and ooo (group (op rhombus...))))
      #:with (::pos-rest)
      #'((group (op &) List (parens e ooo)))))

  (define-splicing-syntax-class :kwp-rest
    #:attributes [kwarg kwparsed]
    #:datum-literals (group op)
    #:literals (~&)
    (pattern (~seq (group (op ~&) a ...))
      #:with kwarg::non-...-binding #'(group a ...)
      #:with kwparsed #'kwarg.parsed
      #:attr arg #'#f
      #:attr parsed #'#f))

  (define-splicing-syntax-class :maybe-arg-rest
    #:attributes [arg parsed kwarg kwparsed]
    #:datum-literals (group op)
    #:literals (& ~& rhombus...)
    (pattern (~seq
              (~alt (~optional ::pos-rest #:defaults ([arg #'#f] [parsed #'#f]))
                    (~optional ::kwp-rest #:defaults ([kwarg #'#f] [kwparsed #'#f])))
              ...))))

(define-syntax fun
  (make-expression+definition-transformer
   (expression-transformer
    #'fun
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group block alts)
        [(form-id (alts-tag::alts
                   (block (group (_::parens arg::kw-binding ... rest::maybe-arg-rest) ret::ret-annotation
                                 (~and rhs (_::block body ...))))
                   ...+)
                  . tail)
         (values
          (build-case-function (get-local-name #'form-id)
                               #'((arg.kw ...) ...)
                               #'((arg ...) ...) #'((arg.parsed ...) ...)
                               #'(rest.arg ...) #'(rest.parsed ...)
                               #'(rest.kwarg ...) #'(rest.kwparsed ...)
                               #'(ret.predicate ...)
                               #'(rhs ...)
                               #'form-id #'alts-tag)
          #'tail)]
        [(form-id (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest) ret::ret-annotation
                  (~and rhs (_::block body ...))
                  . tail)
         (define fun
           (build-function (get-local-name #'form-id)
                           #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                           #'rest.arg #'rest.parsed
                           #'rest.kwarg #'rest.kwparsed
                           #'ret.predicate
                           #'rhs
                           #'form-id #'parens-tag))
         (values (if (pair? (syntax-e #'ret.static-infos))
                     (wrap-static-info fun #'#%call-result #'ret.static-infos)
                     fun)
                 #'tail)])))
   (definition-transformer
     (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group block alts parens)
        [(form-id (alts-tag::alts
                   (block (group name-seq::dotted-identifier-sequence (_::parens arg::kw-binding ... rest::maybe-arg-rest)
                                 ret::ret-annotation
                                 (~and rhs (_::block body ...))))
                   ...+))
         #:with (name::dotted-identifier ...) #'(name-seq ...)
         (define names (syntax->list #'(name.name ...)))
         (define the-name (car names))
         (check-consistent stx names "name")
         (maybe-add-function-result-definition
          the-name (syntax->list #'(ret.static-infos ...))
          (list
           #`(define #,the-name
               #,(build-case-function the-name
                                      #'((arg.kw ...) ...)
                                      #'((arg ...) ...) #'((arg.parsed ...) ...)
                                      #'(rest.arg ...) #'(rest.parsed ...)
                                      #'(rest.kwarg ...) #'(rest.kwparsed ...)
                                      #'(ret.predicate ...)
                                      #'(rhs ...)
                                      #'form-id #'alts-tag))))]
        [(form-id name-seq::dotted-identifier-sequence (parens-tag::parens arg::kw-opt-binding ... rest::maybe-arg-rest)
                  ret::ret-annotation
                  (~and rhs (_::block body ...)))
         #:with name::dotted-identifier #'name-seq
         (maybe-add-function-result-definition
          #'name.name (list #'ret.static-infos)
          (list
           #`(define name.name
               #,(build-function #'name.name
                                 #'(arg.kw ...) #'(arg ...) #'(arg.parsed ...) #'(arg.default ...)
                                 #'rest.arg #'rest.parsed
                                 #'rest.kwarg #'rest.kwparsed
                                 #'ret.predicate
                                 #'rhs
                                 #'form-id #'parens-tag))))]
        ;; definition form didn't match, so try parsing as a `fun` expression:
        [(_ (~or (_::parens _ ...)
                 (_::alts (block (group (parens _ ...) . _)) ...+))
            . _)
         (syntax-parse #`(group . #,stx)
           [e::expression
            (list #'e.parsed)])])))))

(begin-for-syntax
  (define (get-local-name who)
    (or (syntax-local-name) who))

  (struct fcase (kws args arg-parseds rest-arg rest-arg-parsed kwrest-arg kwrest-arg-parsed pred rhs))

  ;; usage: (fcase-pos fcase-args fc) or (fcase-pos fcase-arg-parseds fc)
  (define (fcase-pos get-args fc)
    (for/list ([kw (in-list (fcase-kws fc))]
               [arg (in-list (get-args fc))]
               #:when (not (syntax-e kw)))
      arg))

  (define (build-function function-name
                          kws args arg-parseds defaults
                          rest-arg rest-parsed
                          kwrest-arg kwrest-parsed
                          pred
                          rhs
                          start end)
    (with-syntax-parse ([(arg-parsed::binding-form ...) arg-parseds]
                        [(arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)]
                        [(arg-info::binding-info ...) #'(arg-impl.info ...)])
      (with-syntax ([(tmp-id ...) (generate-temporaries #'(arg-info.name-id ...))]
                    [(arg ...) args]
                    [rhs rhs]
                    [(maybe-rest-tmp (maybe-rest-tmp* ...) maybe-match-rest)
                     (if (syntax-e rest-arg)
                         (with-syntax-parse ([rest::binding-form rest-parsed]
                                             [rest-impl::binding-impl #'(rest.infoer-id () rest.data)]
                                             [rest-info::binding-info #'rest-impl.info])
                           #`(rest-tmp (#:rest rest-tmp) (rest-tmp rest-info #,rest-arg #f)))
                         #'(() () #f))]
                    [((maybe-kwrest-tmp ...) maybe-match-kwrest)
                     (if (syntax-e kwrest-arg)
                         (with-syntax-parse ([kwrest::binding-form kwrest-parsed]
                                             [kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)]
                                             [kwrest-info::binding-info #'kwrest-impl.info])
                           #`((#:kwrest kwrest-tmp) (kwrest-tmp kwrest-info #,kwrest-arg #f)))
                         #'(() #f))])
        (with-syntax ([(((arg-form ...) arg-default) ...)
                       (for/list ([kw (in-list (syntax->list kws))]
                                  [tmp-id (in-list (syntax->list #'(tmp-id ...)))]
                                  [default (in-list (syntax->list defaults))])
                         ;; FIXME: if `default` is simple enough, then
                         ;; use it instead of `unsafe-undefined`, and
                         ;; then `define` has the opportunity to inline it
                         (define arg+default
                           (cond
                             [(not (syntax-e default))
                              tmp-id]
                             [else
                              #`[#,tmp-id unsafe-undefined]]))
                         (cond
                           [(not (syntax-e kw))
                            (list (list arg+default) default)]
                           [else
                            (list (list kw arg+default) default)]))])
          (define body
            #`(nested-bindings
               #,function-name
               #f ; try-next
               argument-binding-failure
               (tmp-id arg-info arg arg-default)
               ...
               maybe-match-rest
               maybe-match-kwrest
               (begin
                 (add-annotation-check
                  #,function-name #,pred
                  (rhombus-body-expression rhs)))))
          (relocate
           (span-srcloc start end)
           (if (syntax-e kwrest-arg)
               #`(lambda/kwrest
                  (arg-form ... ...)
                  maybe-rest-tmp* ... maybe-kwrest-tmp ...
                  #,body)
               #`(lambda (arg-form ... ... . maybe-rest-tmp)
                   #,body)))))))

  (define (build-case-function function-name
                               kwss-stx argss-stx arg-parsedss-stx
                               rest-args-stx rest-parseds-stx
                               kwrest-args-stx kwrest-parseds-stx
                               preds-stx
                               rhss-stx
                               start end)
    (define kwss (map syntax->list (syntax->list kwss-stx)))
    (define argss (map syntax->list (syntax->list argss-stx)))
    (define arg-parsedss (map syntax->list (syntax->list arg-parsedss-stx)))
    (define rest-args (syntax->list rest-args-stx))
    (define rest-parseds (syntax->list rest-parseds-stx))
    (define kwrest-args (syntax->list kwrest-args-stx))
    (define kwrest-parseds (syntax->list kwrest-parseds-stx))
    (define preds (syntax->list preds-stx))
    (define rhss (syntax->list rhss-stx))
    (define n+sames
      (group-by-counts
       (map fcase kwss argss arg-parsedss rest-args rest-parseds kwrest-args kwrest-parseds preds rhss)))
    (define pos-arity
      (normalize-arity
       (for/list ([n+same (in-list n+sames)])
         (define n (car n+same))
         (cond
           [(negative? n) (arity-at-least (- (add1 n)))]
           [else n]))))
    (define allowed-kws
      (cond
        [(ormap syntax-e kwrest-args) #f]
        [else (sort (filter keyword? (apply set-union '() (syntax->datum kwss-stx))) keyword<?)]))
    (define required-kws
      (cond
        [(pair? kwss)
         (sort (filter keyword? (apply set-intersect (syntax->datum kwss-stx))) keyword<?)]
        [else '()]))
    (define kws? (not (null? allowed-kws)))
    (define reduce-keyword-arity
      (cond
        [(null? allowed-kws) values]
        [(and (null? required-kws) (not allowed-kws)) values]
        [else
         (lambda (stx)
           #`(procedure-reduce-keyword-arity/infer-name
              #,stx
              #,(arity->syntax pos-arity)
              '#,required-kws
              '#,allowed-kws))]))
    (relocate
     (span-srcloc start end)
     (reduce-keyword-arity
      #`(case-lambda/kwrest
         #,@(for/list ([n+same (in-list n+sames)])
              (define n (car n+same))
              (define same (cdr n+same))
              (with-syntax ([(try-next pos-arg-id ...) (generate-temporaries
                                                        (cons 'try-next
                                                              (fcase-pos fcase-args (find-matching-case n same))))]
                            [(maybe-rest-tmp ...) (if (negative? n)
                                                      #'(#:rest rest-tmp)
                                                      #'())]
                            [maybe-rest-tmp-use (if (negative? n)
                                                    #'rest-tmp
                                                    #'null)]
                            [(maybe-kwrest-tmp ...) (if kws?
                                                        #'(#:kwrest kwrest-tmp)
                                                        #'())]
                            [maybe-kwrest-tmp-use (if kws?
                                                      #'kwrest-tmp
                                                      #''#hashalw())])
                #`[(pos-arg-id ...)
                   maybe-rest-tmp ... maybe-kwrest-tmp ...
                   #,(let loop ([same same])
                       (cond
                         [(null? same)
                          #`(cases-failure '#,function-name maybe-rest-tmp-use maybe-kwrest-tmp-use pos-arg-id ...)]
                         [else
                          (define fc (car same))
                          (define-values (this-args wrap-adapted-arguments)
                            (adapt-arguments-for-count fc n #'(pos-arg-id ...) #'rest-tmp
                                                       (and kws? #'kwrest-tmp)
                                                       #'try-next))
                          (with-syntax-parse ([(arg ...) (fcase-args fc)]
                                              [(arg-parsed::binding-form ...) (fcase-arg-parseds fc)]
                                              [(arg-impl::binding-impl ...) #'((arg-parsed.infoer-id () arg-parsed.data) ...)]
                                              [(arg-info::binding-info ...) #'(arg-impl.info ...)]
                                              [(this-arg-id ...) this-args]
                                              [pred (fcase-pred fc)]
                                              [rhs (fcase-rhs fc)]
                                              [(maybe-match-rest (maybe-bind-rest-seq ...) (maybe-bind-rest ...))
                                               (cond
                                                 [(syntax-e (fcase-rest-arg fc))
                                                  (define rest-parsed (fcase-rest-arg-parsed fc))
                                                  (with-syntax-parse ([rest::binding-form rest-parsed]
                                                                      [rest-impl::binding-impl #'(rest.infoer-id () rest.data)]
                                                                      [rest-info::binding-info #'rest-impl.info])
                                                    #`((rest-tmp rest-info #,(fcase-rest-arg fc) #f)
                                                       ((rest-info.binder-id rest-tmp rest-info.data))
                                                       ((define-static-info-syntax/maybe rest-info.bind-id rest-info.bind-static-info ...)
                                                        ...)))]
                                                 [else
                                                  #'(#f () ())])]
                                              [(maybe-match-kwrest (maybe-bind-kwrest-seq ...) (maybe-bind-kwrest ...))
                                               (cond
                                                 [(syntax-e (fcase-kwrest-arg fc))
                                                  (define kwrest-parsed (fcase-kwrest-arg-parsed fc))
                                                  (with-syntax-parse ([kwrest::binding-form kwrest-parsed]
                                                                      [kwrest-impl::binding-impl #'(kwrest.infoer-id () kwrest.data)]
                                                                      [kwrest-info::binding-info #'kwrest-impl.info])
                                                    #`((kwrest-tmp kwrest-info #,(fcase-kwrest-arg fc) #f)
                                                       ((kwrest-info.binder-id kwrest-tmp kwrest-info.data))
                                                       ((define-static-info-syntax/maybe kwrest-info.bind-id kwrest-info.bind-static-info ...)
                                                        ...)))]
                                                 [else
                                                  #'(#f () ())])])
                            #`(let ([try-next (lambda () #,(loop (cdr same)))])
                                #,(wrap-adapted-arguments
                                   #`(nested-bindings
                                      #,function-name
                                      try-next
                                      argument-binding-failure
                                      (this-arg-id arg-info arg #f)
                                      ...
                                      maybe-match-rest
                                      maybe-match-kwrest
                                      (begin
                                        (arg-info.binder-id this-arg-id arg-info.data) ...
                                        (begin
                                          (define-static-info-syntax/maybe arg-info.bind-id arg-info.bind-static-info ...)
                                          ...)
                                        ...
                                        maybe-bind-rest-seq ...
                                        maybe-bind-rest ...
                                        maybe-bind-kwrest-seq ...
                                        maybe-bind-kwrest ...
                                        (add-annotation-check
                                         #,function-name
                                         pred
                                         (rhombus-body-expression rhs)))))))]))]))))))

  (define (maybe-add-function-result-definition name static-infoss defns)
    (define (same-expression? a b)
      (cond
        [(identifier? a) (and (identifier? b)
                              (free-identifier=? a b))]
        [(syntax? a) (same-expression? (syntax-e a) b)]
        [(syntax? b) (same-expression? a (syntax-e b))]
        [(pair? a) (and (pair? b)
                        (same-expression? (car a) (car b))
                        (same-expression? (cdr a) (cdr b)))]
        [else (equal? a b)]))
    (cond
      [(and (pair? static-infoss)
            (pair? (syntax-e (car static-infoss)))
            (for/and ([static-infos (in-list (cdr static-infoss))])
              (same-expression? (car static-infoss) static-infos)))
       (cons
        #`(define-static-info-syntax #,name (#%call-result #,(car static-infoss)))
        defns)]
      [else defns]))

  ;; returns (listof (cons n (listof fcase)))
  ;; where `n` is the argument count, and a negative
  ;; `n` means "-(n+1) or more"; although the `n`s
  ;; can be in any order, the `fcase`s are kept in the same
  ;; order within the group for one `n`
  (define (group-by-counts fcases)
    ;; if there is any rest clause, then other clauses
    ;; whose arity overlaps needs to be merged; a rest
    ;; clause requiring at least N arguments will merge
    ;; with any clause that accepts N or more
    (define rest-min
      (for/fold ([rest-min #f]) ([fc (in-list fcases)])
        (cond
          [(syntax-e (fcase-rest-arg fc))
           (define n (length (fcase-pos fcase-args fc)))
           (if rest-min (min rest-min n) n)]
          [else rest-min])))
    (define ht
      (for/fold ([ht #hasheqv()]) ([fc (in-list fcases)])
        (define n (length (fcase-pos fcase-args fc)))
        (define key (if (and rest-min (>= n rest-min))
                        (- (add1 rest-min))
                        n))
        (hash-set ht key (cons fc (hash-ref ht key '())))))
    (for/list ([(key sames) (in-hash ht)])
      (cons key (reverse sames))))

  (define (find-matching-case n same)
    (define find-n (if (negative? n) (- (add1 n)) n))
    (for/or ([fc (in-list same)])
      (define fc-n (length (fcase-pos fcase-args fc)))
      (and (eqv? find-n fc-n)
           fc)))

  ;; Inputs:
  ;;   fc: the fcase to be adapted, with positional arity n'
  ;;   n: the minimum-positional-arity of the case-lambda case to fit into
  ;;   pos-arg-ids-stx: the first n positional arguments
  ;;   rest-tmp: a possible positional-rest that may contain arguments after n
  ;;   kwrest-tmp: a possible keyword-rest
  ;;   try-next: a thunk to try the next fcase within the n case on failure
  ;; Outputs:
  ;;   new-arg-ids: positional and keyword arguments corresponding to fc
  ;;   wrap-adapted-arguments: to bind new-arg-ids, rest-tmp, and kwrest-tmp, or fail
  ;; when a clause that expects n' (or more) arguments is merged
  ;; with a clause that expects n or more arguments (so n <= n'), then
  ;; the rest argument needs to be unpacked to extra arguments
  (define (adapt-arguments-for-count fc n pos-arg-ids-stx rest-tmp kwrest-tmp try-next)
    (define base-f-n (length (fcase-pos fcase-args fc)))
    (define f-n (if (syntax-e (fcase-rest-arg fc))
                    (- (add1 base-f-n))
                    base-f-n))
    ;; adapt single arguments
    (define-values (_empty new-arg-ids-rev wrap/single-args)
      (for/fold ([pos-arg-ids-rem (syntax->list pos-arg-ids-stx)]
                 [new-arg-ids-rev '()]
                 [wrap values])
                ([kw (in-list (fcase-kws fc))]
                 [arg (in-list (fcase-args fc))])
        (cond
          [(and (not (syntax-e kw)) (pair? pos-arg-ids-rem))
           (values (cdr pos-arg-ids-rem)
                   (cons (car pos-arg-ids-rem) new-arg-ids-rev)
                   wrap)]
          [(not (syntax-e kw))
           (unless (negative? n) (error "assert failed in wrap-adapted: n"))
           (define tmp (generate-temporary arg))
           (values pos-arg-ids-rem
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      #`(if (pair? #,rest-tmp)
                            (let ([#,tmp (car #,rest-tmp)])
                              (let ([#,rest-tmp (cdr #,rest-tmp)])
                                #,body))
                            (#,try-next)))))]
          [else
           (unless kwrest-tmp (error "assert failed in wrap-adapted: kwrest-tmp 1"))
           (define tmp (generate-temporary arg))
           (values pos-arg-ids-rem
                   (cons tmp new-arg-ids-rev)
                   (lambda (body)
                     (wrap
                      #`(if (hash-has-key? #,kwrest-tmp '#,kw)
                            (let ([#,tmp (hash-ref #,kwrest-tmp '#,kw)])
                              (let ([#,kwrest-tmp (hash-remove #,kwrest-tmp '#,kw)])
                                #,body))
                            (#,try-next)))))])))
    (unless (null? _empty) (error "assert failed in wrap-adapted: pos-arg-ids-rem"))
    ;; check empty positional rest if it exists in the n case but not the fcase
    (define wrap/rest
      (cond
        [(eqv? n f-n) wrap/single-args]
        [(negative? f-n) wrap/single-args]
        [else
         (unless (negative? n) (error "assert failed in wrap-adapted: n"))
         (lambda (body)
           (wrap/single-args
            #`(if (null? #,rest-tmp)
                  (let () #,body)
                  (#,try-next))))]))
    ;; check empty keyword rest if it exists in the kwrest-tmp but not the fcase
    (define wrap/kwrest
      (cond
        [(and (not kwrest-tmp) (not (syntax-e (fcase-kwrest-arg fc))))
         wrap/rest]
        [(and kwrest-tmp (syntax-e (fcase-kwrest-arg fc)))
         wrap/rest]
        [else
         (unless kwrest-tmp (error "assert failed in wrap-adapted: kwrest-tmp 2"))
         (lambda (body)
           (wrap/rest
            #`(if (hash-empty? #,kwrest-tmp)
                  (let () #,body)
                  (#,try-next))))]))
    (values (reverse new-arg-ids-rev)
            wrap/kwrest)))

(define (argument-binding-failure who val annotation-str)
  (raise-binding-failure who "argument" val annotation-str))

(define (cases-failure who rest-args kwrest-args . base-args)
  (define args (append base-args rest-args))
  (apply
   raise-contract-error
   who
   (apply string-append "no matching case for arguments\n"
          "  arguments...:"
          (for/list ([arg (in-list args)])
            "\n   ~e"))
   args))

(define-syntax (add-annotation-check stx)
  (syntax-parse stx
    [(_ name pred e)
     (cond
       [(syntax-e #'pred)
        #`(let ([result e])
            (if (pred result)
                result
                (result-failure 'name result)))]
       [else #'e])]))

(define (result-failure who val)
  (raise-contract-error
   who
   (string-append "result does not match annotation\n"
                  "  result: ~v")
   val))

(begin-for-syntax
  (define-syntax-class :kw-expression
    #:datum-literals (op block group)
    (pattern (group kw:keyword (block (group e ...)))
             #:with exp::expression #'(group e ...)
             #:attr parsed #'exp.parsed)
    (pattern exp::expression
             #:attr kw #'#f
             #:attr parsed #'exp.parsed)))

(define-for-syntax (parse-function-call rator-in stxes)
  (define rator (rhombus-local-expand rator-in))
  (syntax-parse stxes
    #:datum-literals (group op)
    #:literals (& ~& rhombus...)
    [(_ (head::parens rand ...) . tail)
     #:when (complex-argument-splice? #'(rand ...))
     (values (complex-argument-splice-call rator-in #'head #'(rand ...))
             #'tail)]
    [(_ (head::parens rand ...
                      (group (op &) rst ...)
                      (group (op ~&) kwrst ...))
        . tail)
     (generate-call rator #'head #'(rand ...) #'(group rst ...) #f
                    #'(group kwrst ...) #t
                    #'tail)]
    [(_ (head::parens rand ...
                      rep (group (op (~and dots rhombus...)))
                      (group (op ~&) kwrst ...))
        . tail)
     (generate-call rator #'head #'(rand ...) #'rep #'dots
                    #'(group kwrst ...)
                    #'tail)]
    [(_ (head::parens rand ... (group (op &) rst ...)) . tail)
     (generate-call rator #'head #'(rand ...) #'(group rst ...) #f #f #'tail)]
    [(_ (head::parens rand ... rep (group (op (~and dots rhombus...)))) . tail)
     (generate-call rator #'head #'(rand ...) #'rep #'dots #f #'tail)]
    [(_ (head::parens rand ... (group (op ~&) kwrst ...)) . tail)
     (generate-call rator #'head #'(rand ...) #f #f #'(group kwrst ...) #'tail)]
    [(_ (head::parens rand ...) . tail)
     (generate-call rator #'head #'(rand ...) #f #f #f #'tail)]))

(define-for-syntax (generate-call rator head rands rsts dots kwrsts tail)
  (with-syntax-parse ([(rand::kw-expression ...) rands])
    (with-syntax-parse ([((arg-form ...) ...) (for/list ([kw (in-list (syntax->list #'(rand.kw ...)))]
                                                         [parsed (in-list (syntax->list #'(rand.parsed ...)))])
                                                (if (syntax-e kw)
                                                    (list kw parsed)
                                                    (list parsed)))])
      (define e
        (cond
          [kwrsts
           (define kwrest-args
             (with-syntax-parse ([kwrst::expression kwrsts]) #'kwrst.parsed))
           (define rest-args
             (cond
               [dots (repetition-as-list dots rsts 1)]
               [rsts (with-syntax-parse ([rst::expression rsts]) #'rst.parsed)]
               [else #''()]))
           (datum->syntax (quote-syntax here)
                          (list (append (list #'keyword-apply/map rator)
                                        (syntax->list #'(arg-form ... ...))
                                        (list rest-args))
                                kwrest-args)
                          (span-srcloc rator head)
                          head)]
          [rsts
           (define rest-args
             (cond 
               [dots (repetition-as-list dots rsts 1)]
               [else (with-syntax-parse ([rst::expression rsts]) #'rst.parsed)]))
           (datum->syntax (quote-syntax here)
                          (append (list #'apply rator)
                                  (syntax->list #'(arg-form ... ...))
                                  (list rest-args))
                          (span-srcloc rator head)
                          head)]
          [else (datum->syntax (quote-syntax here)
                               (cons rator #'(arg-form ... ...))
                               (span-srcloc rator head)
                               head)]))
      (define result-static-infos (or (syntax-local-static-info rator #'#%call-result)
                                      #'()))
      (values (wrap-static-info* e result-static-infos)
              tail))))

(define-for-syntax (complex-argument-splice? gs-stx)
  ;; multiple `&` or `...`, or not at the end before `~&`,
  ;; or `~&` that's not at the very end?
  (define (not-kw-splice-only? gs-stx)
    (syntax-parse gs-stx
      #:datum-literals (group op)
      #:literals (~&)
      [((group (op ~&) rand ...+)) #f]
      [() #f]
      [_ #t]))
  (let loop ([gs-stx gs-stx])
    (syntax-parse gs-stx
      #:datum-literals (group op)
      #:literals (& ~& rhombus...)
      [() #f]
      [((group (op &) rand ...+) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [(g0 (group (op rhombus...)) . gs)
       (or (loop #'gs) (not-kw-splice-only? #'gs))]
      [((group (op (~and splice ~&)) rand ...+) . gs)
       (or (loop #'(g . gs))  (pair? (syntax-e #'gs)))]
      [(_ . gs) (loop #'gs)])))

(define-for-syntax (complex-argument-splice-call rator head gs-stx)
  (define (gen-id) (car (generate-temporaries '(arg))))
  (let loop ([gs-stx gs-stx]
             [rev-args '()])
    (syntax-parse gs-stx
      #:datum-literals (group op)
      #:literals (& ~& rhombus...)
      [()
       (define args (reverse rev-args))
       #`(let #,(for/list ([arg (in-list args)])
                  #`[#,(car arg) #,(caddr arg)])
             #,(let ([lists? (for/or ([arg (in-list args)])
                               (eq? 'list (cadr arg)))])
                 (define-values (term ignored-tail)
                   (generate-call rator head
                                  (append
                                   (if lists?
                                       null
                                       (for/list ([arg (in-list args)]
                                                  #:when (eq? (cadr arg) 'arg))
                                         #`(group (parsed #,(car arg)))))
                                   (for/list ([arg (in-list args)]
                                              #:when (eq? (cadr arg) 'kw))
                                     #`(group #,(list-ref arg 3)
                                              (block (group (parsed #,(car arg)))))))
                                  (and lists?
                                       #`(group
                                          (parsed
                                           (append
                                            #,@(for/list ([arg (in-list args)]
                                                          #:when (or (eq? (cadr arg) 'arg)
                                                                     (eq? (cadr arg) 'list)))
                                                 (cond
                                                   [(eq? (cadr arg) 'arg)
                                                    #`(list #,(car arg))]
                                                   [else
                                                    (car arg)]))))))
                                  #f
                                  (let ([kwss (for/list ([arg (in-list args)]
                                                         #:when (eq? (cadr arg) 'kws))
                                                (car arg))])
                                    (cond
                                      [(null? kwss) #f]
                                      [(null? (cdr kwss)) #`(group (parsed #,(car kwss)))]
                                      [else #`(group (parsed (merge-keyword-argument-maps #,@kwss)))]))
                                  #'#f))
                 term))]
      [(((~and tag group) (op &) rand ...+) . gs)
       (loop #'gs
             (cons (list (gen-id) 'list #'(rhombus-expression (tag rand ...)) #f)
                   rev-args))]
      [(g0 (group (op (~and dots rhombus...))) . gs)
       (loop #'gs
             (cons (list (gen-id) 'list (repetition-as-list #'dots #'g0 1))
                   rev-args))]
      [(((~and tag group) (op ~&) rand ...+) . gs)
       (loop #'gs
             (cons (list (gen-id) 'kws #'(rhombus-expression (tag rand ...)))
                   rev-args))]
      [((group kw:keyword (tag::block body ...)) . gs)
       (loop #'gs
             (cons (list (gen-id) 'kw #'(rhombus-body-at tag body ...) #'kw)
                   rev-args))]
      [(g . gs)
       (loop #'gs
             (cons (list (gen-id) 'arg #'(rhombus-expression g))
                   rev-args))])))

(define function-call-who '|function call|)

(define keyword-apply/map
  (make-keyword-procedure
   (lambda (kws kw-args proc . args+rest)
     ;; currying makes it easier to preserve order when `~&` is last
     (lambda (kw-ht)
       (define all-kw-ht
         (for/fold ([ht kw-ht]) ([kw (in-list kws)]
                                 [arg (in-list kw-args)])
           (when (hash-ref kw-ht kw #f)
             (raise-arguments-error* function-call-who rhombus-realm
                                     "duplicate keyword in spliced map and direct keyword arguments"
                                     "keyword" kw))
           (hash-set ht kw arg)))
       (define all-kws (sort (append kws (hash-keys kw-ht)) keyword<?))
       (keyword-apply proc
                      all-kws
                      (for/list ([kw (in-list all-kws)])
                        (hash-ref all-kw-ht kw))
                      (let loop ([args+rest args+rest])
                        (cond
                          [(null? (cdr args+rest)) (car args+rest)]
                          [else (cons (car args+rest) (loop (cdr args+rest)))])))))))

(define (merge-keyword-argument-maps ht . hts)
  (define (check-hash ht)
    (unless (hash? ht)
      (raise-arguments-error* function-call-who rhombus-realm
                              "not a map for keyword arguments"
                              "given" ht)))
  (check-hash ht)
  (for/fold ([accum-ht ht]) ([ht (in-list hts)])
    (check-hash ht)
    (for/fold ([accum-ht accum-ht]) ([(kw arg) (in-hash ht)])
      (when (hash-ref accum-ht kw #f)
        (raise-arguments-error* function-call-who rhombus-realm
                                "duplicate keyword in in keyword-argument maps"
                                "keyword" kw))
      (hash-set accum-ht kw arg))))
