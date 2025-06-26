#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/syntax-local
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "dotted-sequence.rkt"
                     "pack.rkt")
         "expression.rkt"
         (submod "annotation.rkt" for-class)
         (submod "syntax-class-primitive.rkt" for-quasiquote)
         (submod "dot.rkt" for-dot-provider)
         "dollar.rkt"
         "repetition.rkt"
         "static-info.rkt"
         (submod "syntax-object.rkt" for-quasiquote)
         "syntax-wrap.rkt"
         "dot-provider-key.rkt"
         "syntax-class-attributes-key.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parens.rkt"
         "pack.rkt")

(provide (for-syntax make-pattern-variable-bind
                     deepen-pattern-variable-bind
                     extract-pattern-variable-bind-id-and-depth
                     normalize-pvar-statinfos
                     get-syntax-class-static-infos
                     build-wrap-syntax-for-attributes
                     keep-syntax-wrap))

(define-for-syntax (make-pattern-variable-bind name-id temp-id unpack* depth
                                               #:statinfos [statinfos (get-syntax-static-infos)]
                                               #:attribs [attrib-lists '()]
                                               #:key [key #f])
  (define no-repetition?
    (and (eqv? 0 depth)
         (null? attrib-lists)))
  (define ids (if no-repetition?
                  (list name-id)
                  (list name-id (in-repetition-space name-id))))
  #`[#,ids (make-pattern-variable-syntaxes
             (quote-syntax #,name-id)
             (quote-syntax #,temp-id)
             (quote-syntax #,unpack*)
             #,depth
             (quote-syntax #,statinfos)
             (quote-syntax #,attrib-lists)
             #,no-repetition?
             (quote #,key))
     #,@statinfos])

(define-for-syntax (deepen-pattern-variable-bind sidr)
  (syntax-parse sidr
    [(ids (make-pattern-variable-syntaxes self-id temp-id unpack* depth statinfos attrs expr? key) . sis)
     (define new-ids
       (syntax-parse #'ids
         [(id) #`(id #,(in-repetition-space #'id))]
         [_ #'ids]))
     #`(#,new-ids (make-pattern-variable-syntaxes self-id temp-id unpack* #,(add1 (syntax-e #'depth)) statinfos attrs #f key) . sis)]))

(define-for-syntax (extract-pattern-variable-bind-id-and-depth sids sid-ref)
  (list (car (syntax-e sids))
        (syntax-parse sid-ref
          [(make-pattern-variable-syntaxes _ _ _ depth . _) #'depth])))


(define-for-syntax (get-syntax-class-static-infos statinfos attributes)
  (cond
    [(null? (syntax-e attributes))
     statinfos]
    [else
     (define dp/s (extract-dot-provider-ids (static-info-lookup (normalize-pvar-statinfos statinfos)
                                                                #'#%dot-provider)))
     (define stx-dp-id (get-syntax-instances))
     (if (for/or ([id (in-list dp/s)])
           (free-identifier=? (in-dot-provider-space id)
                              (in-dot-provider-space stx-dp-id)))
         #`((#%dot-provider ((pattern-variable-dot-provider #,stx-dp-id) #,stx-dp-id))
            (#%syntax-class-attributes #,attributes))
         statinfos)]))

(define-for-syntax (build-attribute-hash attributes #:vars [ids #f])
  (define vars (map syntax-list->pattern-variable (syntax->list attributes)))
  (define key-uses (for/fold ([counts #hasheq()]) ([var (in-list vars)])
                     (hash-update counts (pattern-variable-sym var) add1 0)))
  (define (multi-use? key) ((hash-ref key-uses key) . > . 1))
  #`(hasheq
     #,@(apply
         append
         (append
          (for/list ([key (in-hash-keys key-uses)]
                     #:when (multi-use? key))
            (list #`(quote #,key) #'(quote ambiguous)))
          (for/list ([var (in-list vars)]
                     [id (in-list (or ids (map pattern-variable-val-id vars)))]
                     #:unless (multi-use? (pattern-variable-sym var)))
            (list #`(quote #,(pattern-variable-sym var))
                  #`(cons #,id #,(pattern-variable-depth var))))))))

(define-for-syntax (make-pattern-variable-syntaxes name-id temp-id unpack* depth statinfos attributes no-repetition? key)
  (define expr-handler
    (lambda (stx fail)
      (if (eqv? depth 0)
          (id-handler stx)
          (fail))))
  (define id-handler
    (lambda (stx)
      (syntax-parse stx
        [(_ . tail) (values (if (null? (syntax-e attributes))
                                (wrap-static-info* temp-id (get-syntax-static-infos))
                                (wrap-static-info* #`(maybe-syntax-wrap
                                                      (syntax-unwrap #,temp-id)
                                                      0
                                                      (quote #,key)
                                                      #,(build-attribute-hash attributes))
                                                   statinfos))
                            #'tail)])))
  (cond
    [no-repetition?
     (if (null? (syntax-e attributes))
         (expression-repeatable-transformer
          id-handler)
         (expression-repeatable-transformer
          (lambda (stx)
            (expr-handler stx
                          (lambda ()
                            (id-handler stx))))))]
    [else
     (define attrs (syntax->list attributes))
     (define attr-tmps (generate-temporaries attrs))
     (make-expression+repetition
      (cond
        [(= depth 0)
         #'()]
        [(null? attrs)
         #`(([(elem) (in-list (#,unpack* #'$ #,temp-id #,depth))])
            #,@(for/list ([i (in-range (sub1 depth))])
                 #`([(elem) (in-list elem)])))]
        [else
         #`(([(elem) (in-list (#,unpack* #'$ #,temp-id #,depth))]
             #,@(for/list ([var (in-list attrs)]
                           [tmp (in-list attr-tmps)])
                  (define attr (syntax-list->pattern-variable var))
                  #`[(#,tmp) (in-list #,(pattern-variable-val-id attr))]))
            #,@(for/list ([i (in-range (sub1 depth))])
                 #`([(elem) (in-list elem)]
                    #,@(for/list ([tmp (in-list attr-tmps)])
                         #`[(#,tmp) (in-list #,tmp)]))))])
      (cond
        [(= depth 0)
         (let-values ([(e tail) (id-handler #'(x))])
           e)]
        [(null? attrs)
         #'elem]
        [else
         #`(maybe-syntax-wrap
            elem
            0
            (quote #,key)
            #,(build-attribute-hash (datum->syntax #f attrs) #:vars attr-tmps))])
      (lambda () statinfos)
      #:repet-handler (lambda (stx next) (next))
      #:expr-handler expr-handler)]))

(define-for-syntax (build-wrap-syntax-for-attributes base-stx key attributes)
  (cond
    [(and key (pair? (syntax-e attributes)))
     ;; some patterns in "quasiquote.rkt" rely on the #`(maybe-syntax-wrap #,base-stx #,depth . _) shape
     #`(maybe-syntax-wrap #,base-stx
                          0
                          (quote #,key)
                          #,(build-attribute-hash attributes))]
    [else base-stx]))

(define-syntax pattern-variable-dot-provider
  (dot-provider
   (lambda (form1 dot field-id
                  tail
                  more-static?
                  repetition?
                  success-k fail-k)
     (define-values (attributes depth)
       (cond
         [repetition?
          (syntax-parse form1
            [rep-info::repetition-info
             (values (static-info-lookup #'rep-info.element-static-infos #'#%syntax-class-attributes)
                     (length (syntax->list #'rep-info.for-clausess)))])]
         [else
          (values
           (or (syntax-local-static-info form1 #'#%syntax-class-attributes)
               #'())
           0)]))
     (define attrs (for/list ([var (in-list (syntax->list attributes))]
                              #:when (eq? (syntax-e field-id) (syntax-e (car (syntax-e var)))))
                     (syntax-list->pattern-variable var)))
     (define attr (and (pair? attrs) (car attrs)))
     (when (and (pair? attrs) (pair? (cdr attrs)))
       (raise-syntax-error #f
                           "field name is ambiguous"
                           (datum->syntax #f (list form1 dot field-id))
                           field-id))
     (cond
       [attr
        (unless (or repetition?
                    (eqv? 0 (+ depth (pattern-variable-depth attr))))
          (raise-syntax-error #f
                              "field is a repetition"
                              (datum->syntax #f (list form1 dot field-id))
                              field-id))
        (values
         (cond
           [repetition?
            (define var-depth (pattern-variable-depth attr))
            (syntax-parse form1
              [form-rep-info::repetition-info
               (define e #`(#,(keep-syntax-wrap (pattern-variable-unpack* attr))
                            #'$
                            (car (hash-ref (syntax-wrap-attribs form-rep-info.body)
                                           (quote #,(pattern-variable-sym attr))))
                            #,var-depth))
               (make-repetition-info (respan (datum->syntax #f (list form1 dot field-id)))
                                     (cond
                                       [(= var-depth 0)
                                        #'form-rep-info.for-clausess]
                                       [else
                                        #`(#,@#'form-rep-info.for-clausess
                                           ([(elem) (in-list #,e)])
                                           #,@(for/list ([i (in-range (sub1 var-depth))])
                                                #`([(elem) (in-list elem)])))])
                                     (cond
                                       [(= var-depth 0) e]
                                       [else #'elem])
                                     (normalize-pvar-statinfos (pattern-variable-statinfos attr))
                                     0)])]
           [else
            (wrap-static-info* #`(car (hash-ref (syntax-wrap-attribs #,(discard-static-infos form1))
                                                (quote #,(pattern-variable-sym attr))))
                               (normalize-pvar-statinfos (pattern-variable-statinfos attr)))])
         tail)]
       [else
        (fail-k)]))))

(begin-for-syntax
  (set-parse-syntax-of-annotation!
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (_::parens (group seq::dotted-identifier-sequence)) . tail)
        (define (bad [constraint ""])
          (raise-syntax-error #f (string-append "not a syntax class" constraint) stx #'seq))
        (syntax-parse #'seq
          [(~var name (:hier-name-seq in-name-root-space in-syntax-class-space name-path-op name-root-ref))
           (define rsc (syntax-local-value* (in-syntax-class-space #'name.name) syntax-class-ref))
           (cond
             [rsc
              (unless (rhombus-syntax-class-key rsc)
                (bad " with fields"))
              (define (root-swap attrs)
                (cond
                  [(rhombus-syntax-class-root-swap rsc)
                   => (lambda (swap)
                        (datum->syntax
                         #f
                         (cons
                          (pattern-variable->list
                           (pattern-variable (cdr swap)
                                             'dummy 'dummy
                                             0
                                             (case (rhombus-syntax-class-kind rsc)
                                               [(group) #'unpack-group*]
                                               [(term) (if (rhombus-syntax-class-splicing? rsc)
                                                           #'unpack-element*
                                                           #'unpack-term*)]
                                               [else #'unpack-element*])
                                             'stx))
                          (for/list ([attr (in-list (syntax->list attrs))]
                                     #:do [(define pv (syntax-list->pattern-variable attr))]
                                     #:unless (eq? (car swap) (pattern-variable-sym pv)))
                            attr))))]
                  [else attrs]))
              (define statinfos
                (cond
                  [(rhombus-syntax-class-root-swap rsc)
                   => (lambda (swap)
                        (or (for/or ([attr (in-list (syntax->list (rhombus-syntax-class-attributes rsc)))])
                              (define pv (syntax-list->pattern-variable attr))
                              (and (eq? (car swap) (pattern-variable-sym pv))
                                   (normalize-pvar-statinfos (pattern-variable-statinfos pv))))
                            #'()))]
                  [else (get-syntax-static-infos)]))
              (values
               (annotation-predicate-form #`(lambda (v)
                                              (and (syntax-wrap? v)
                                                   (eq? (syntax-wrap-key v) (quote #,(rhombus-syntax-class-key rsc)))))
                                          (get-syntax-class-static-infos statinfos
                                                                         (root-swap (rhombus-syntax-class-attributes rsc))))
               #'tail)]
             [else (bad)])]
          [else (bad)])]))))

(define-for-syntax (normalize-pvar-statinfos pvar-sis)
  (if (eq? pvar-sis 'stx)
      (get-syntax-static-infos)
      pvar-sis))

;; When packing to communicate a match as a syntax-class attribute,
;; we don't want to discard syntax wraps, because those wraps are
;; useful when a syntax-class field itself is from a syntax class
(define-for-syntax (keep-syntax-wrap unpack*)
  (case (syntax-e unpack*)
    [(unpack-term* unpack-multi-as-term* unpack-group*)
     (quote-syntax unpack-element*)]
    [else unpack*]))
