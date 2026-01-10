#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/property
                     enforest/syntax-local
                     "introducer.rkt"
                     "srcloc.rkt"
                     (for-syntax racket/base))
         "dotted-sequence-parse.rkt")

;; Represent static information in either of two ways:
;;
;;   - bind an identifier (in some space) to a `static-info` record,
;;     which maps keys to values; the static info applies to a use of
;;     the identifier
;;
;;   - wrap a form with `(begin (quote-syntax (key val)) form)`, where
;;     the pattern can be nested in `form`, and the static info
;;     applies to the form

(begin-for-syntax
  (provide (property-out static-info)
           (property-out static-info-key)
           static-info-get-stxs
           in-static-info-space
           wrap-static-info
           wrap-static-info*
           :static-info
           syntax-local-static-info
           extract-static-infos
           normalize-static-infos
           normalize-static-infos/values
           unwrap-static-infos
           discard-static-infos
           relocate-wrapped
           static-info-lookup
           static-infos-or
           static-infos-and
           static-info-identifier-or
           static-info-identifier-and
           static-infos-result-or
           static-infos-result-and
           static-infos-remove
           static-infos-flatten
           get-dependent-result-proc
           get-empty-static-infos
           static-infos-empty?))

(provide define-static-info-getter
         define-static-info-syntax
         define-static-info-syntaxes
         define-static-info-syntax/maybe
         define-static-info-syntax/maybe/maybe-extension

         define-static-info-key-syntax/provide

         #%indirect-static-info
         #%values
         #%maybe
         #%none)

(begin-for-syntax
  (property static-info (get-stxs))
  (property static-info-key (or and))

  (define in-static-info-space (make-interned-syntax-introducer/add 'rhombus/statinfo))

  (define (wrap-static-info expr key-id val-stx)
    (let ([expr (maybe-respan expr)])
      (relocate+reraw
       expr
       #`(begin (quote-syntax (#,key-id #,val-stx))
                #,expr)
       #:prop-stx expr)))

  (define (wrap-static-info* expr stxes)
    (for/foldr ([expr expr]) ([stx (in-list (if (syntax? stxes)
                                                (syntax->list stxes)
                                                stxes))])
      (syntax-parse stx
        [(key:identifier val) (wrap-static-info expr #'key #'val)])))

  (define-syntax-class (:static-info key-id)
    #:attributes (val)
    #:literals (begin quote-syntax)
    (pattern id:identifier
             #:do [(define v (syntax-local-value* (in-static-info-space #'id)
                                                  static-info-ref))
                   (define val (and v
                                    (static-info-lookup ((static-info-get-stxs v)) key-id)))]
             #:when val
             #:with val val)
    (pattern (begin (quote-syntax (key:identifier val)) _)
             #:when (free-identifier=? #'key key-id))
    (pattern (begin (quote-syntax (key:identifier indirect-id)) _)
             #:when (free-identifier=? #'key #'#%indirect-static-info)
             #:do [(define val (indirect-static-info-ref #'indirect-id key-id))]
             #:when val
             #:with val val)
    (pattern (begin (quote-syntax (_:identifier _)) (~var || (:static-info key-id)))))

  (define (syntax-local-static-info expr key-id)
    (syntax-parse expr
      [(~var dp (:static-info key-id))
       #'dp.val]
      [_ #f]))

  (define (indirect-static-info-ref id key-id)
    (syntax-local-static-info id key-id))

  (define (extract-static-infos e)
    (let loop ([e e])
      (syntax-parse e
        #:literals (begin quote-syntax)
        [id:identifier
         (cond
           [(syntax-local-value* (in-static-info-space #'id)
                                 static-info-ref)
            => (lambda (v)
                 ((static-info-get-stxs v)))]
           [else null])]
        [(begin (quote-syntax (~and form (_:identifier _))) e)
         (cons #'form (loop #'e))]
        [_ null])))

  (define (normalize-static-infos infos)
    (car (normalize-static-infos/values 1 infos)))

  (define (normalize-static-infos/values num infos)
    (define infoss
      (syntax-parse infos
        #:literals (#%values)
        [((#%values (si ...))) (syntax->list #'(si ...))]
        [_ (list infos)]))
    (if (eqv? (length infoss) num)
        (for/list ([infos (in-list infoss)])
          (let loop ([infos infos])
            (syntax-parse infos
              #:literals (#%values)
              [((#%values (only-infos))) (loop #'only-infos)]
              [((#%values _)) #'()]
              [_ infos])))
        (for/list ([_ (in-range num)])
          #'())))

  ;; use on sub-expressions to discover otherwise immediate forms that
  ;; are hidden
  (define (unwrap-static-infos e)
    (define unwrapped-e
      (let loop ([e e] [unwrapped? #f])
        (syntax-parse e
          #:literals (begin quote-syntax)
          [(begin (quote-syntax (_:identifier _)) e) (loop #'e #t)]
          [_ (and unwrapped? e)])))
    (if unwrapped-e
        ;; we need to track origin here to transfer any potential
        ;; information added by enforestation, but don't merge raw
        (reraw e (syntax-track-origin unwrapped-e e #'begin))
        e))

  ;; use on sub-expressions when constructing a parsed primitive form
  ;; with the goal of simplifying the result expansion
  (define (discard-static-infos e)
    (unwrap-static-infos e))

  (define (static-info-lookup static-infos find-key
                              #:no-indirect? [no-indirect? #f])
    (for/or ([static-info (in-list (if (syntax? static-infos)
                                       (syntax->list static-infos)
                                       static-infos))])
      (syntax-parse static-info
        [(key val) (or (and (free-identifier=? #'key find-key)
                            #'val)
                       (and (not no-indirect?)
                            (free-identifier=? #'key #'#%indirect-static-info)
                            (indirect-static-info-ref #'val find-key)))]
        [_ #f])))

  ;; it's better to relocate and then wrap, since wrapping propagates
  ;; the location, but sometimes it's so much easier to relocate
  ;; afterward that it's worth the extra cost
  (define (relocate-wrapped srcloc e)
    (syntax-parse e
      #:literals (begin quote-syntax)
      [((~and tag begin) (~and qs (quote-syntax (_:identifier _))) e)
       (define e2 (relocate-wrapped srcloc #'e))
       (relocate e2 #`(tag qs #,e2) e2)]
      [_ (relocate+reraw srcloc e)])))

(define-syntax (define-static-info-key-syntax/provide stx)
  (syntax-parse stx
    [(_ id:identifier rhs)
     #`(begin
         (define-syntax id rhs)
         (provide id))]))

(define-syntax #%indirect-static-info
  (static-info-key (lambda (a b) (error "should not ``or'' indirect statinfos"))
                   (lambda (a b) (error "should not ``and'' indirect statinfos"))))

(define-syntax #%values
  (let ([merge (lambda (a b combine)
                 (define as (syntax->list a))
                 (define bs (syntax->list b))
                 (and as bs (equal? (length as) (length bs))
                      (datum->syntax
                       #f
                       (map combine as bs))))])
    (static-info-key (lambda (a b)
                       (merge a b static-infos-or))
                     (lambda (a b)
                       (merge a b static-infos-and)))))

(define-syntax #%none
  (static-info-key (lambda (a b) a)
                   (lambda (a b) a)))

(define-for-syntax (make-static-info-getter stx)
  (define (->compact rhss)
    (for/list ([rhs (in-list rhss)])
      ;; recognizes some common patterns to generate
      ;; code that's slightly more compact
      (syntax-parse rhs
        [(us:identifier e)
         #:when (free-transformer-identifier=? #'us #'unsyntax)
         #'e]
        [(key (us:identifier val))
         #:when (free-transformer-identifier=? #'us #'unsyntax)
         #'(datum->syntax #f (list (quote-syntax key) val))]
        [rhs #'(quasisyntax rhs)])))
  (syntax-parse stx
    [(rhs ... us:identifier rhs*)
     #:when (free-transformer-identifier=? #'us #'unsyntax)
     #`(lambda () (list* #,@(->compact (syntax->list #'(rhs ...))) rhs*))]
    [(rhs ... (us*:identifier rhs*))
     #:when (free-transformer-identifier=? #'us* #'unsyntax-splicing)
     #`(lambda () (list* #,@(->compact (syntax->list #'(rhs ...))) rhs*))]
    [(rhs ...)
     #`(lambda () (list #,@(->compact (syntax->list #'(rhs ...)))))]))

(define-syntax (define-static-info-getter stx)
  (syntax-parse stx
    [(_ id:identifier . tail)
     #`(define-for-syntax id
         #,(make-static-info-getter #'tail))]))

(define-syntax (define-static-info-syntax stx)
  (syntax-parse stx
    [(_ id:identifier #:getter getter:identifier)
     #`(define-syntax #,(in-static-info-space #'id)
         (static-info getter))]
    [(_ id:identifier . tail)
     #`(define-syntax #,(in-static-info-space #'id)
         (static-info #,(make-static-info-getter #'tail)))]))

(define-syntax (define-static-info-syntaxes stx)
  (syntax-parse stx
    [(_ (id:identifier ...) #:getter getter:identifier)
     #'(begin
         (define-static-info-syntax id #:getter getter)
         ...)]
    [(_ (id:identifier ...) . tail)
     #'(begin
         (define-static-info-getter getter . tail)
         (define-static-info-syntax id #:getter getter)
         ...)]))

(define-syntax (define-static-info-syntax/maybe stx)
  (syntax-parse stx
    [(_ id) #'(begin)]
    [(_ id rhs ...) #'(define-static-info-syntax id rhs ...)]))

(define-syntax (define-static-info-syntax/maybe/maybe-extension stx)
  (syntax-parse stx
    [(_ id prefix) #'(begin)]
    [(_ id prefix rhs ...)
     (if (syntax-e #'prefix)
         (build-syntax-definition/maybe-extension
          'rhombus/statinfo #'id #'prefix
          #`(static-info #,(make-static-info-getter #'(rhs ...))))
         #'(define-static-info-syntax id rhs ...))]))

(define-static-info-getter get-empty-static-infos)

(define-for-syntax (static-infos-empty? si)
  (or (null? si) (and (syntax? si) (null? (syntax-e si)))))

(define-for-syntax (flatten-indirects as)
  (and as
       (for*/list ([a (in-list as)]
                   [e (in-list
                       (syntax-parse a
                         [(a-key a-val)
                          #:when (free-identifier=? #'a-key #'#%indirect-static-info)
                          (define si (and (identifier? #'a-val)
                                          (syntax-local-value* (in-static-info-space #'a-val)
                                                               static-info-ref)))
                          (if si
                              (flatten-indirects ((static-info-get-stxs si)))
                              null)]
                         [_ (list a)]))])
         e)))

(define-for-syntax (static-infos-flatten as)
  (flatten-indirects as))

;; note that `||` at the annotation level feels like "intersection" on statinfo tables
(define-for-syntax (static-infos-or as bs)
  (cond
    [(static-infos-empty? as) as]
    [(static-infos-empty? bs) bs]
    [else
     (let ([as (flatten-indirects (if (syntax? as) (syntax->list as) as))]
           [bs (flatten-indirects (if (syntax? bs) (syntax->list bs) bs))])
       (define (merge)
         ;; special generalization of `maybe`
         (define ma (and as (static-info-lookup as (quote-syntax #%maybe))))
         (define mb (and bs (static-info-lookup bs (quote-syntax #%maybe))))
         (let ([as (if (and mb (not ma))
                       (cons #`(#%maybe #,as) as)
                       as)]
               [bs (if (and ma (not mb))
                       (cons #`(#%maybe #,bs) bs)
                       bs)])
           (or
            (and as
                 bs
                 (for/list ([a (in-list as)]
                            #:do [(define new-val
                                    (syntax-parse a
                                      [(a-key a-val)
                                       (for/or ([b (in-list bs)])
                                         (syntax-parse b
                                           [(b-key b-val)
                                            #:when (free-identifier=? #'a-key #'b-key)
                                            (let ([key (syntax-local-value* #'a-key static-info-key-ref)])
                                              (cond
                                                [key
                                                 ((static-info-key-or key) #'a-val #'b-val)]
                                                [else
                                                 (static-infos-result-or #'a-val #'b-val)]))]
                                           [_ #f]))]
                                      [_ #f]))]
                            #:when new-val)
                   (syntax-parse a
                     [(a-key . _) (datum->syntax #f (list #'a-key new-val))])))
            #'())))
       (define (none? as) (and as (static-info-lookup as (quote-syntax #%none))))
       ;; if one has `None` and the other doesn't, ignore the one with `None`
       (cond
         [(none? as) (if (none? bs)
                         (merge)
                         bs)]
         [(none? bs) as]
         [else (merge)]))]))

;; note that `&&` at the annotation level feels like "union" on statinfo tables
(define-for-syntax static-infos-and
  (case-lambda
    [(as bs)
     (cond
       [(static-infos-empty? as) bs]
       [(static-infos-empty? bs) as]
       [else
        (let ([as (flatten-indirects (if (syntax? as) (syntax->list as) as))]
              [bs (flatten-indirects (if (syntax? bs) (syntax->list bs) bs))])
          (cond
            [(not as) (or bs #'())]
            [(not bs) as]
            [else
             (append
              (for/list ([a (in-list as)]
                         #:do [(define new-val
                                 (syntax-parse a
                                   [(a-key a-val)
                                    (define v
                                      (for/or ([b (in-list bs)])
                                        (syntax-parse b
                                          [(b-key b-val)
                                           #:when (free-identifier=? #'a-key #'b-key)
                                           (let ([key (syntax-local-value* #'a-key static-info-key-ref)])
                                             (list
                                              (cond
                                                [key
                                                 ((static-info-key-and key) #'a-val #'b-val)]
                                                [else
                                                 (static-infos-result-and #'a-val #'b-val)])))]
                                          [_ #f])))
                                    (if v
                                        (car v)
                                        #'a-val)]
                                   [_ #f]))]
                         #:when new-val)
                (syntax-parse a
                  [(a-key . _) (datum->syntax #f (list #'a-key new-val))]))
              (for/list ([b (in-list bs)]
                         #:unless (syntax-parse b
                                    [(b-key . _)
                                     (for/or ([a (in-list as)])
                                       (syntax-parse a
                                         [(a-key b-val)
                                          (free-identifier=? #'a-key #'b-key)]
                                         [_ #f]))]
                                    [_ #f]))
                b))]))])]
    ;; 3-argument version is useful in code generated by "class-static-info.rkt"
    [(as bs cs) (static-infos-and (static-infos-and as bs) cs)]))

(define-for-syntax static-info-identifier-and
  (lambda (a b)
    ;; biased toward `a`
    a))

(define-for-syntax static-info-identifier-or
  (lambda (a b)
    (and (identifier? a)
         (identifier? b)
         (free-identifier=? a b)
         a)))

(define-for-syntax (static-infos-result-and as bs)
  ;; With `#:at_arities`, for now, we handle only the simple case that the masks coincide
  (syntax-parse as
    [(#:at_arities (a-mask a-results) ...)
     (syntax-parse bs
       [(#:at_arities (b-mask b-results) ...)
        (if (equal? (syntax->datum #'(a-mask ...)) (syntax->datum #'(b-mask ...)))
            #`(#:at_arities #,(for/list ([a-mask (in-list (syntax->list #'(a-mask ...)))]
                                         [a-results (in-list (syntax->list #'(a-results ...)))]
                                         [b-results (in-list (syntax->list #'(b-results ...)))])
                                #`(,a-mask #,(static-infos-maybe-dependent-result-and a-results b-results))))
            as)]
       [_
        as])]
    [_
     (syntax-parse bs
       [(#:at_arities (b-mask b-results) ...)
        as]
       [_ (static-infos-maybe-dependent-result-and as bs)])]))

(define-for-syntax (static-infos-result-or as bs)
  ;; With `#:at_arities`, for now, we handle only the simple case that the masks coincide
  (syntax-parse as
    [(#:at_arities (a-mask a-results) ...)
     (syntax-parse bs
       [(#:at_arities (b-mask b-results) ...)
        (if (equal? (syntax->datum #'(a-mask ...)) (syntax->datum #'(b-mask ...)))
            #`(#:at_arities #,(for/list ([a-mask (in-list (syntax->list #'(a-mask ...)))]
                                         [a-results (in-list (syntax->list #'(a-results ...)))]
                                         [b-results (in-list (syntax->list #'(b-results ...)))])
                                #`(#,a-mask #,(static-infos-maybe-dependent-result-or a-results b-results))))
            #f)]
       [_
        #f])]
    [_
     (syntax-parse bs
       [(#:at_arities (b-mask b-results) ...)
        #f]
       [_ (static-infos-maybe-dependent-result-or as bs)])]))

(define-static-info-key-syntax/provide #%maybe
  (static-info-key static-infos-result-or
                   static-infos-result-and))

(define-for-syntax (merge-dependent-results as bs merge-dependent-id merge)

  (syntax-parse as
    #:literals (#%dependent-result)
    [((#%dependent-result a-clos))
     (syntax-parse bs
       #:literals (#%dependent-result)
       [((#%dependent-result b-clos))
        #`((#%dependent-result (#,merge-dependent-id (a-clos b-clos))))]
       [_
        #`((#%dependent-result (#,merge-dependent-id (a-clos (independent #,bs)))))])]
    [_
     (syntax-parse bs
       #:literals (#%dependent-result)
       [((#%dependent-result b-clos))
        #`((#%dependent-result (#,merge-dependent-id ((independent #,as) b-clos))))]
       [_ (merge as bs)])]))

(define-for-syntax (static-infos-maybe-dependent-result-and as bs)
  (merge-dependent-results as bs #'merge-dependent-and static-infos-and))

(define-for-syntax (static-infos-maybe-dependent-result-or as bs)
  (merge-dependent-results as bs #'merge-dependent-or static-infos-or))

(define-syntax independent
  (lambda (data deps)
    data))

(define-for-syntax (get-dependent-result-proc id)
  (define proc (syntax-local-value* id (lambda (v)
                                         (and (procedure? v)
                                              v))))
  (unless proc
    (raise-syntax-error #f
                        "cannot find a transformer for a dependent result"
                        id))
  proc)

(define-syntax merge-dependent-and
  (lambda (data deps)
    (syntax-parse data
      [((a-proc-id a-data) (b-proc-id b-data))
       (define a-proc (get-dependent-result-proc #'a-proc-id))
       (define b-proc (get-dependent-result-proc #'b-proc-id))
       (static-infos-and (a-proc #'a-data deps) (b-proc #'b-data deps))])))

(define-syntax merge-dependent-or
  (lambda (data deps)
    (syntax-parse data
      [((a-proc-id a-data) (b-proc-id b-data))
       (define a-proc (get-dependent-result-proc #'a-proc-id))
       (define b-proc (get-dependent-result-proc #'b-proc-id))
       (static-infos-or (a-proc #'a-data deps) (b-proc #'b-data deps))])))

(define-for-syntax (static-infos-dependent-result-and a-clos b-clos)
  #`(merge-dependent-and (#,a-clos #,b-clos)))

(define-for-syntax (static-infos-dependent-result-or a-clos b-clos)
  #`(merge-dependent-or (#,a-clos #,b-clos)))

(define-static-info-key-syntax/provide #%dependent-result
  (static-info-key static-infos-dependent-result-or
                   static-infos-dependent-result-and))

(define-for-syntax (static-infos-remove as key)
  (for/list ([a (in-list (if (syntax? as) (syntax->list as) as))]
             #:when (syntax-parse a
                      [(a-key a-val) (not (free-identifier=? #'a-key key))]))
    a))

(define-for-syntax (equal-static-info-value? a b)
  (cond
    [(identifier? a)
     (and (identifier? b)
          (free-identifier=? a b))]
    [(identifier? b) #f]
    [(syntax? a)
     (equal-static-info-value? (syntax-e a) b)]
    [(syntax? b)
     (equal-static-info-value? a (syntax-e b))]
    [(null? a) (null? b)]
    [(pair? a)
     (and (pair? b)
          (and (equal-static-info-value? (car a) (car b))
               (equal-static-info-value? (cdr a) (cdr b))))]
    [(vector? a)
     (and (vector? b)
          (= (vector-length a) (vector-length b))
          (for/and ([ae (in-vector a)]
                    [be (in-vector b)])
            (equal-static-info-value? (car ae) (car be))))]
    [(box? a)
     (and (box? b)
          (equal-static-info-value? (unbox a) (unbox b)))]
    [else (equal? a b)]))
