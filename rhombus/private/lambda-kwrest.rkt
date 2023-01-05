#lang racket/base

(provide lambda/kwrest
         case-lambda/kwrest
         hash-remove*
         procedure-reduce-keyword-arity-mask/infer-name
         (for-syntax arity->mask-syntax))

(require (only-in racket/list drop)
         (only-in racket/set subset?)
         (only-in racket/unsafe/undefined unsafe-undefined)
         (for-syntax racket/base
                     (only-in racket/function normalize-arity)
                     racket/list
                     racket/match
                     (only-in racket/set set-intersect set-union)
                     (only-in racket/syntax generate-temporary)
                     (only-in syntax/name syntax-local-infer-name)
                     syntax/parse
                     (only-in syntax/parse [attribute @])
                     (only-in syntax/parse/lib/function-header formals-no-rest)))

;; ---------------------------------------------------------

(begin-for-syntax
  (define-syntax-class pos-param
    #:attributes [param id mand opt-id
                  param/tmp expr/tmp default]
    [pattern {~and mand:id id:id
                   param param/tmp}
      #:attr expr/tmp #f
      #:attr opt-id #f
      #:attr default #f]
    [pattern {~and [{~and opt-id:id id:id} default:expr]
                   param}
      #:with tmp (generate-temporary #'id)
      #:with param/tmp
      (syntax/loc #'param [tmp unsafe-undefined])
      #:with expr/tmp #'(if (eq? tmp unsafe-undefined) default tmp)
      #:attr mand #f])

  (define-splicing-syntax-class kw-param
    #:attributes [kw id mand-kw default]
    [pattern {~seq {~and kw:keyword mand-kw:keyword} id:id}
      #:attr default #f]
    [pattern {~seq kw:keyword [id:id default:expr]}
      #:attr mand-kw #f])

  (define-splicing-syntax-class single-param
    #:attributes [pos.param pos.id pos.mand pos.opt-id
                  pos.param/tmp pos.expr/tmp pos.default
                  kwp.kw kwp.id kwp.mand-kw kwp.default]
    [pattern {~seq pos:pos-param}
      #:attr kwp.kw #f
      #:attr kwp.id #f
      #:attr kwp.mand-kw #f
      #:attr kwp.default #f]
    [pattern kwp:kw-param
      #:attr pos.param #f
      #:attr pos.id/tmp #f
      #:attr pos.param/tmp #f
      #:attr pos.expr/tmp #f
      #:attr pos.id #f
      #:attr pos.mand #f
      #:attr pos.opt-id #f
      #:attr pos.default #f])

  (define-splicing-syntax-class single-params
    #:auto-nested-attributes
    [pattern {~and _:formals-no-rest {~seq :single-param ...} {~seq stx ...}}
      #:attr pos-mand-N (length (syntax->list #'({~? pos.mand} ...)))
      #:attr pos-opt-N (length (syntax->list #'({~? pos.opt-id} ...)))
      #:attr pos-all-N (+ (@ pos-mand-N) (@ pos-opt-N))
      #:with pos-mand-N-stx #`'#,(@ pos-mand-N)
      #:with pos-all-N-stx #`'#,(@ pos-all-N)
      #:attr [pos-index 1]
      (for/fold ([acc '()] [i 0] #:result (reverse acc))
                ([x (in-list (@ pos.id))])
        (if x (values (cons i acc) (add1 i)) (values (cons #f acc) i)))
      #:attr [pos-index-stx 1]
      (for/list ([i (in-list (@ pos-index))])
        (and i #`'#,i))
      #:attr mand-kws (sort (syntax->datum #'({~? kwp.mand-kw} ...)) keyword<?)])

  (define-splicing-syntax-class params/kwrest
    #:auto-nested-attributes
    [pattern {~seq (s:single-params)
                   {~alt {~optional {~seq #:rest r:id}}
                         {~optional {~seq #:kwrest kwr:id}}}
                   ...}
      #:attr pos-arity
      (cond
        [(@ r) (arity-at-least (@ s.pos-mand-N))]
        [(< (@ s.pos-mand-N) (@ s.pos-all-N))
         (inclusive-range (@ s.pos-mand-N) (@ s.pos-all-N))]
        [else (@ s.pos-mand-N)])
      #:attr allowed-kws
      (cond
        [(@ kwr) #f]
        [else (sort (syntax->datum #'({~? s.kwp.kw} ...)) keyword<?)])])

  (define-syntax-class (pos-arity-case-tmps arity)
    #:attributes [(s 2) (r 1)]
    [pattern _
      #:do [(define cs (flatten arity))]
      #:with [(s:id ... . {~or r:id ()}) ...]
      (for/list ([c (in-list cs)])
        (match c
          [(? exact-nonnegative-integer? n) (generate-temporaries (make-list n 's))]
          [(arity-at-least n) (append (generate-temporaries (make-list n 's))
                                      (generate-temporary 'r))]))])

  (define (arity->mask-syntax a)
    #`#,(let loop ([a (normalize-arity a)])
          (cond
            [(arity-at-least? a) (bitwise-xor -1 (sub1 (arithmetic-shift 1 (arity-at-least-value a))))]
            [(exact-nonnegative-integer? a) (arithmetic-shift 1 a)]
            [(pair? a) (bitwise-ior (loop (car a)) (loop (cdr a)))]
            [(null? a) 0]))))

;; ---------------------------------------------------------

(define-syntax lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      [(_ (s:single-params) {~optional {~seq #:rest r:id}} b:expr ...+)
       #'(lambda (s.stx ... . {~? r ()}) b ...)]
      [(_ :params/kwrest
          b:expr
          ...+)
       #:with name:id (or (syntax-local-infer-name stx) #'proc)
       #:attr kw-mand-error
       (match (@ s.mand-kws)
         ['() #f]
         [(list kw) #`(raise-required-keyword-not-supplied 'name '#,kw)]
         [kws #`(raise-required-keywords-not-supplied 'name '#,kws)])
       #:attr pos-arity-mask-stx (arity->mask-syntax (@ pos-arity))
       #:attr kw-mand-arity (and (pair? (@ s.mand-kws)) #`'#,(@ s.mand-kws))
       #:attr kw-allowed-arity (and (pair? (@ s.mand-kws)) #`'#,(@ allowed-kws))
       #:with {~var case-tmps (pos-arity-case-tmps (@ pos-arity))} #f
       #:with kwhash-proc (car (generate-temporaries '(kwhash-proc)))
       #'(let* ([kwhash-proc
                 (lambda (kwhash {~? s.pos.param/tmp} ... . {~? r ()})
                   (let*
                       ([kwr (hash-remove* kwhash '({~? s.kwp.kw} ...))]
                        {~? [s.pos.id s.pos.expr/tmp]
                            {~? [s.kwp.id
                                 (hash-ref kwhash 's.kwp.kw
                                           (λ ()
                                             {~? s.kwp.default
                                                 (raise-required-keyword-not-supplied 'name 's.kwp.mand-kw)}))]}}
                        ...)
                     b
                     ...))]
                [name
                 (case-lambda
                   [(case-tmps.s ... . {~? case-tmps.r ()})
                    {~? kw-mand-error
                        (apply kwhash-proc '#hashalw() case-tmps.s ...
                               {~? case-tmps.r '()})}]
                   ...)])
           {~? (procedure-reduce-keyword-arity-mask
                (make-keyword-hash-procedure kwhash-proc name)
                pos-arity-mask-stx
                kw-mand-arity
                kw-allowed-arity)
               (make-keyword-hash-procedure kwhash-proc name)})])))

(define-syntax case-lambda/kwrest
  (lambda (stx)
    (syntax-parse stx
      [(_ [c:params/kwrest
           b:expr
           ...+]
          ...)
       #:with name:id (or (syntax-local-infer-name stx) #'proc)
       #:do [(define pos-arity (normalize-arity (flatten (@ c.pos-arity))))
             (define mand-kws
               (cond
                 [(pair? (@ c.s.mand-kws))
                  (sort (apply set-intersect (@ c.s.mand-kws)) keyword<?)]
                 [else '()]))
             (define overall-kws
               (and (andmap values (@ c.allowed-kws))
                    (sort (apply set-union '() (@ c.allowed-kws)) keyword<?)))
             (define reduce-kws? (or (pair? mand-kws) overall-kws))]
       #:attr kw-mand-error
       (match mand-kws
         ['() #f]
         [(list kw) #`(raise-required-keyword-not-supplied 'name '#,kw)]
         [kws #`(raise-required-keywords-not-supplied 'name '#,kws)])
       #:attr pos-arity-mask-stx (arity->mask-syntax pos-arity)
       #:attr kw-mand-arity (and reduce-kws? #`'#,mand-kws)
       #:attr kw-allowed-arity (and reduce-kws? #`'#,overall-kws)
       #:with {~var case-tmps (pos-arity-case-tmps pos-arity)} #f
       #:with kwhash-proc (car (generate-temporaries '(kwhash-proc)))
       #'(let* ([kwhash-proc
                 (lambda (kwhash . lst)
                   (define N (length lst))
                   (cond
                     [(and {~? (begin 'c.r (<= c.s.pos-mand-N-stx N))
                               (<= c.s.pos-mand-N-stx N c.s.pos-all-N-stx)}
                           (hash-has-keys? kwhash '({~? c.s.kwp.mand-kw} ...))
                           {~? 'c.kwr
                               (subset? (hash-keys kwhash) '({~? c.s.kwp.kw} ...))})
                      (let*
                          ({~? [c.r (drop lst (min N c.s.pos-all-N-stx))]}
                           {~? [c.kwr (hash-remove* kwhash '({~? c.s.kwp.kw} ...))]}
                           {~?
                            [c.s.pos.opt-id (if (< c.s.pos-index-stx N)
                                                (list-ref lst c.s.pos-index-stx)
                                                c.s.pos.default)]
                            {~?
                             [c.s.pos.mand (list-ref lst c.s.pos-index-stx)]
                             {~?
                              [c.s.kwp.id
                               (hash-ref kwhash 'c.s.kwp.kw
                                         (λ ()
                                           {~? c.s.kwp.default
                                               (raise-required-keyword-not-supplied 'name
                                                                                    'c.s.kwp.mand-kw)}))]}}}
                           ...)
                        b
                        ...)]
                     ...
                     [else (raise-no-case-matches 'name)]))]
                [name
                 (case-lambda
                   [(case-tmps.s ... . {~? case-tmps.r ()})
                    {~? kw-mand-error
                        (apply kwhash-proc '#hashalw() case-tmps.s ...
                               {~? case-tmps.r '()})}]
                   ...)])
           {~? (procedure-reduce-keyword-arity-mask
                (make-keyword-hash-procedure kwhash-proc name)
                pos-arity-mask-stx
                kw-mand-arity
                kw-allowed-arity)
               (make-keyword-hash-procedure kwhash-proc name)})])))

(define-syntax procedure-reduce-keyword-arity-mask/infer-name
  (lambda (stx)
    (syntax-parse stx
      [(_ f:expr pos-arity-mask:expr req-kws:expr all-kws:expr)
       #:with name:id (or (syntax-local-infer-name stx) #'proc)
       #'(procedure-reduce-keyword-arity-mask
          (let ([name f]) name)
          pos-arity-mask
          req-kws
          all-kws)])))

;; ---------------------------------------------------------

(define (raise-required-keyword-not-supplied name kw)
  (raise-arguments-error name
                         "required keyword argument not supplied"
                         "required keyword" kw))

(define (raise-required-keywords-not-supplied name kws)
  (raise-arguments-error name
                         "required keyword arguments not supplied"
                         "required keywords" kws))

(define (raise-no-case-matches name)
  (raise-arguments-error name "no case matches"))

;; ---------------------------------------------------------

(define (make-keyword-hash-procedure kwhash-proc
                                     [proc (lambda ps (apply kwhash-proc '#hashalw() ps))])
  (make-keyword-procedure
   (lambda (ks vs . ps)
     (apply kwhash-proc (keyword-lists->hash ks vs) ps))
   proc))

;; keyword-lists->hash : (Listof Keyword) (Listof V) -> (Hashof Keyword V)
(define (keyword-lists->hash ks vs)
  (make-immutable-hashalw (map cons ks vs)))

;; hash-remove* : (Hashof K V) (Listof K) -> (Hashof K V)
(define (hash-remove* h ks)
  (for/fold ([acc h]) ([k (in-list ks)])
    (hash-remove acc k)))

;; hash-has-keys? : (Hashof K V) (Listof K) -> Boolean
(define (hash-has-keys? h ks)
  (for/and ([k (in-list ks)]) (hash-has-key? h k)))
