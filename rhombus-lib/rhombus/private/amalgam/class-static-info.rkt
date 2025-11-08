#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "class-parse.rkt"
                     "static-info-pack.rkt"
                     "annot-context.rkt")
         "entry-point.rkt"
         (submod "function.rkt" for-info)
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "function-arity.rkt"
         "dot-provider-key.rkt"
         "indirect-static-info-key.rkt"
         "static-info.rkt"
         "class-able.rkt"
         "class-forward-annot.rkt")

(provide (for-syntax extract-instance-static-infoss
                     build-instance-static-infos-defs
                     build-class-static-infos)
         define-constructor-static-info)

(define-for-syntax (extract-instance-static-infoss name-id options super interfaces
                                                   private-interfaces protected-interfaces
                                                   internal-id
                                                   dot-providers internal-dot-providers
                                                   intro)
  (define call-statinfo-indirect-id
    (able-statinfo-indirect-id 'call super interfaces name-id intro))
  (define index-statinfo-indirect-id
    (able-statinfo-indirect-id 'get super interfaces name-id intro))
  (define index-set-statinfo-indirect-id
    (able-statinfo-indirect-id 'set super interfaces name-id intro))
  (define append-statinfo-indirect-id
    (able-statinfo-indirect-id 'append super interfaces name-id intro))
  (define compare-statinfo-indirect-id
    (able-statinfo-indirect-id 'compare super interfaces name-id intro))
  (define contains-statinfo-indirect-id
    (able-statinfo-indirect-id 'contains super interfaces name-id intro))

  (define super-call-statinfo-indirect-id
    (able-super-statinfo-indirect-id 'call super interfaces))

  (define static-infos-exprs (hash-ref options 'static-infoss '()))
  (define static-infos-id (and (pair? static-infos-exprs)
                               (intro (datum->syntax #f (string->symbol
                                                         (format "~a-statinfo" (syntax-e name-id)))))))

  (define (get-instance-static-infos-expr internal?)
    (define most-static-infos
      (for/list ([intf (in-list (if super (cons super interfaces) interfaces))]
                 #:unless (and (not internal?)
                               (or
                                (hash-ref private-interfaces intf #f)
                                (hash-ref protected-interfaces intf #f)))
                 #:unless (null? (syntax-e (objects-desc-static-infos intf))))
        (objects-desc-static-infos intf)))
    (cond
      [(and (or (null? most-static-infos)
                (null? (cdr most-static-infos)))
            (not static-infos-id))
       (if (null? most-static-infos)
           #'()
           (car most-static-infos))]
      [(null? most-static-infos)
       #`((#%indirect-static-info #,static-infos-id))]
      [else
       #`((#,(quote-syntax unsyntax-splicing)
           (static-infos-and
            #,@(if static-infos-id
                   #`(#'((#%indirect-static-info #,static-infos-id)))
                   #'())
            #,@(for/list ([si (in-list most-static-infos)])
                 #`(#,(quote-syntax quasisyntax) #,si)))))]))

  (define instance-static-infos-expr (get-instance-static-infos-expr #f))
  (define internal-instance-static-infos-expr (if internal-id
                                                  (get-instance-static-infos-expr #t)
                                                  #'()))

  (define instance-static-infos-id (and (pair? (syntax-e instance-static-infos-expr))
                                        (intro (datum->syntax #f (string->symbol
                                                                  (format "~a-instance-statinfo" (syntax-e name-id)))))))
  (define internal-instance-static-infos-id (and (pair? (syntax-e internal-instance-static-infos-expr))
                                                 (intro (datum->syntax #f (string->symbol
                                                                           (format "~a-internal-instance-statinfo" (syntax-e name-id)))))))

  (define instance-static-infos
    (if instance-static-infos-id
        #`((#%indirect-static-info #,instance-static-infos-id))
        #'()))

  (define internal-instance-static-infos
    (if internal-instance-static-infos-id
        #`((#%indirect-static-info #,internal-instance-static-infos-id))
        #'()))

  (define common-indirect-static-infos
    ;; assuming nothing to merge among these static-info sets
    #`(#,@(if call-statinfo-indirect-id
              #`((#%indirect-static-info #,call-statinfo-indirect-id)
                 #,@(get-function-static-infos))
              #'())
       #,@(if index-statinfo-indirect-id
              #`((#%indirect-static-info #,index-statinfo-indirect-id))
              #'())
       #,@(if index-set-statinfo-indirect-id
              #`((#%indirect-static-info #,index-set-statinfo-indirect-id))
              #'())
       #,@(if append-statinfo-indirect-id
              #`((#%indirect-static-info #,append-statinfo-indirect-id))
              #'())
       #,@(if compare-statinfo-indirect-id
              #`((#%indirect-static-info #,compare-statinfo-indirect-id))
              #'())
       #,@(if contains-statinfo-indirect-id
              #`((#%indirect-static-info #,contains-statinfo-indirect-id))
              #'())))

  (define (build-dot-provider-merge dot-providers
                                    instance-static-infos
                                    name-template)
    (define dp #`((#%dot-provider #,dot-providers)))
    (cond
      [(and (null? (syntax-e instance-static-infos))
            (not call-statinfo-indirect-id))
       (values #f #f #`(#,@dp
                        ;; no dot providers to merge/chain
                        #,@common-indirect-static-infos))]
      [else
       (define id (and (or (pair? (syntax-e instance-static-infos))
                           (pair? (syntax-e common-indirect-static-infos)))
                       (intro (datum->syntax #f (string->symbol
                                                 (format name-template (syntax-e name-id)))))))
       (values id
               #`((#,(quote-syntax unsyntax-splicing)
                   (static-infos-and #'#,dp
                                     #,@(if (pair? (syntax-e instance-static-infos))
                                            #`((#,(quote-syntax quasisyntax) #,instance-static-infos))
                                            null)
                                     #,@(if (pair? (syntax-e common-indirect-static-infos))
                                            #`((#,(quote-syntax quasisyntax) #,common-indirect-static-infos))
                                            null))))
               #`((#%indirect-static-info #,id)))]))

  (define-values (dot-static-infos-id dot-static-infos-expr dot-static-infos)
    (build-dot-provider-merge dot-providers
                              instance-static-infos
                              "~a-dot-statinfo"))

  (define-values (internal-dot-static-infos-id internal-dot-static-infos-expr internal-dot-static-infos)
    (build-dot-provider-merge internal-dot-providers
                              internal-instance-static-infos
                              "~a-internal-dot-statinfo"))

  (define all-static-infos dot-static-infos)
  (define internal-all-static-infos internal-dot-static-infos)

  (values call-statinfo-indirect-id
          index-statinfo-indirect-id
          index-set-statinfo-indirect-id
          append-statinfo-indirect-id
          compare-statinfo-indirect-id
          contains-statinfo-indirect-id

          super-call-statinfo-indirect-id

          ;; has only statinfos provided by `static_info` class clause:
          static-infos-id       ; defined by `build-instance-static-infos-defs`
          static-infos-exprs    ; RHS of definition

          ;; has `static_info` class clause merged with supers:
          instance-static-infos-id   ; defined by `build-instance-static-infos-defs`
          instance-static-infos-expr ; RHS of definition
          instance-static-infos      ; only saved in `class-desc`

          ;; ditto, but for inetrnal name:
          internal-instance-static-infos-id   ; defined by `build-instance-static-infos-defs`
          internal-instance-static-infos-expr ; RHS of definition

          ;; includes indirects for primitive interfaces, and also
          ;; merges in dot provider and "indirect"s:
          dot-static-infos-id   ; defined by `build-instance-static-infos-defs`
          dot-static-infos-expr ; RHS of definition

          ;; ditto, but for an internal name:
          internal-dot-static-infos-id   ; defined by `build-instance-static-infos-defs`
          internal-dot-static-infos-expr ; RHS of definition

          ;; refers to `[internal-]dot-static-infos-id` --- or inlines if simple enough
          ;; so that no separate definition is needed --- and does not include any
          ;; unquotes:
          all-static-infos
          internal-all-static-infos))

(define-for-syntax (build-instance-static-infos-defs static-infos-id static-infos-exprs
                                                     instance-static-infos-id instance-static-infos-expr
                                                     internal-instance-static-infos-id internal-instance-static-infos-expr
                                                     dot-static-infos-id dot-static-infos-expr
                                                     internal-dot-static-infos-id internal-dot-static-infos-expr)
  (define (make-lazy id expr)
    (if id
        (list
         #`(define-syntax #,(in-static-info-space id)
             ;; perform merge laziy
             (let ([si #f])
               (static-info (lambda ()
                              (unless si
                                (set! si (#,(quote-syntax quasisyntax) #,expr)))
                              (if (syntax? si) (syntax->list si) si))))))
        null))
  (append
   (if static-infos-id
       (list
        #`(define-syntax #,(in-static-info-space static-infos-id)
            ;; evaluate `expr` eagerly, since it's provided by the user
            (let ([si (#,(quote-syntax quasisyntax)
                       (#,@(for/list ([expr (in-list (reverse static-infos-exprs))])
                             #`(#,(quote-syntax unsyntax-splicing) (pack-static-infos 'static_info #,expr)))))])
              (static-info (lambda () (syntax->list si))))))
       null)
   (make-lazy instance-static-infos-id instance-static-infos-expr)
   (make-lazy internal-instance-static-infos-id internal-instance-static-infos-expr)
   (make-lazy dot-static-infos-id dot-static-infos-expr)
   (make-lazy internal-dot-static-infos-id internal-dot-static-infos-expr)))

(define-for-syntax (build-class-static-infos exposed-internal-id
                                             super
                                             given-constructor-rhs
                                             constructor-keywords constructor-defaults
                                             constructor-accessors constructor-mutables
                                             constructor-private-keywords constructor-private-defaults
                                             constructor-private-accessors constructor-private-mutables
                                             auto-constructor? constructor-forward-rets
                                             names
                                             #:veneer? [veneer? #f])
  (with-syntax ([(name constructor-name name-instance
                       internal-name-instance make-internal-name
                       all-static-infos internal-all-static-infos
                       [name-field ...]
                       [field-static-infos ...]
                       [public-name-field ...]
                       [public-name-field/mutate ...] [public-maybe-set-name-field! ...]
                       [public-field-static-infos ...])
                 names])
    (append
     (if (syntax-e #'constructor-name)
         (with-syntax ([arity-mask
                        (cond
                          [given-constructor-rhs
                           (syntax-parse given-constructor-rhs
                             [(_ e-arity::entry-point-shape)
                              (hash-ref (or (syntax->datum #'e-arity.parsed) #hasheq()) 'arity #f)])]
                          [veneer? #'2]
                          [else (summarize-arity constructor-keywords
                                                 constructor-defaults
                                                 #f #f)])])
           (define-values (define-static-info-syntax-id def-extras dep-results)
             (cond
               [auto-constructor?
                (with-syntax ([pos+accessors
                               (for/fold ([i 0] [l null] #:result l)
                                         ([kw (in-list constructor-keywords)]
                                          [accessor (in-list constructor-accessors)]
                                          [mutable? (in-list constructor-mutables)])
                                 (values (if (and kw (syntax-e kw))
                                             i
                                             (add1 i))
                                         (if (not (if (syntax? mutable?) (syntax-e mutable?) mutable?))
                                             (cons (if (and kw (syntax-e kw))
                                                       (list kw accessor)
                                                       (list i accessor))
                                                   l)
                                             l)))])
                  (values #'define-static-info-syntax
                          null
                          #'((#%dependent-result (select-for-constructor pos+accessors)))))]
               [else
                (values #'define-constructor-static-info
                        (list constructor-forward-rets)
                        #'())]))
           (with-syntax ([define-static-info-syntax define-static-info-syntax-id]
                         [(extra ...) def-extras]
                         [(dep-result ...) dep-results]
                         [(all-static-info ...) #'all-static-infos])
             (list
              #`(define-static-info-syntax constructor-name
                  extra ...
                  (#%call-result (dep-result ... all-static-info ...))
                  (#%function-arity arity-mask)
                  . (#,(quote-syntax unsyntax) (get-function-static-infos))))))
         null)
     (if (and exposed-internal-id
              (syntax-e #'make-internal-name))
         (list
          (with-syntax ([result-infos
                         (let* ([infos #'internal-all-static-infos]
                                [infos (if super
                                           (with-syntax ([(info ...) infos])
                                             #'((#%call-result (info ... . #,(get-function-static-infos)))))
                                           infos)])
                           infos)])
            #'(define-static-info-syntax make-internal-name
                (#%call-result result-infos)
                . #,(get-function-static-infos))))
         '())
     ;; a `name-field` is not public, so not need for static info:
     #;
     (list
      #'(begin
          (define-static-info-syntax/maybe* name-field
            (#%call-result field-static-infos)
            . #,(get-function-static-infos))
          ...))
     (with-syntax ([(sis ...) (for/list ([maybe-set (in-list (syntax->list #'(public-maybe-set-name-field! ...)))]
                                         [si (in-list (syntax->list #'(public-field-static-infos ...)))]
                                         [public-name-field-id (in-list (syntax->list #'(public-name-field ...)))])
                                (with-syntax ([(info ...)
                                               (if (syntax-e maybe-set)
                                                   (list #`(#%call-result (#:at_arities ((2 ((#%dependent-result (select-field #,public-name-field-id))
                                                                                             #,@si)))))
                                                         #'(#%function-arity 6))
                                                   (list #`(#%call-result ((#%dependent-result (select-field #,public-name-field-id))
                                                                           #,@si))
                                                         #'(#%function-arity 2)))])
                                  #'(info ... . #,(get-function-static-infos))))])
       (list
        #'(begin
            (define-static-info-syntax/maybe* public-name-field/mutate . sis)
            ...))))))

;; drops empty `#%call-result`:
(define-syntax (define-static-info-syntax/maybe* stx)
  (syntax-parse stx
    [(~or* (_ id (_ ()) . tail)
           (_ id (_ (#:at_arities ((_ ())))) . tail)
           (_ id . tail))
     #'(define-static-info-syntax id . tail)]))

(define-syntax (select-field data deps)
  (define accessor-id data)
  (define obj-i 0)
  (define args (annotation-dependencies-args deps))
  (or (static-info-lookup (or (and (< obj-i (length args))
                                   (list-ref args obj-i))
                              #'())
                          accessor-id)
      #'()))

(define-syntax (select-for-constructor data deps)
  (for/fold ([si #'()]) ([d (in-list (syntax->list data))])
    (syntax-parse d
      [(pos-stx accessor-id)
       (define pos (syntax-e #'pos-stx))
       (define new-si
         (cond
           [(keyword? pos)
            (hash-ref (annotation-dependencies-kw-args deps) pos #'())]
           [(pos . < . (length (annotation-dependencies-args deps)))
            (list-ref (annotation-dependencies-args deps) pos)]
           [else #'()]))
       (if (static-infos-empty? new-si)
           si
           #`((accessor-id  #,new-si) . #,si))])))

;; To delay expansion of `constructor-forward-rets` until after
;; the class namespace is ready
(define-syntax (define-constructor-static-info stx)
  (syntax-parse stx
    #:datum-literals (#%call-result)
    [(_ constructor-name
        constructor-forward-rets
        (#%call-result (c ...))
        other-static-info ...)
     (syntax-parse (merge-forwards #'() #'constructor-forward-rets #'#t)
       [((new-static-info ...) _ ([forward-id forward-c-parsed] ...))
        #`(begin
            #,@(build-forward-annotations #'(forward-id ...)
                                          #'(forward-c-parsed ...))
            (define-static-info-syntax constructor-name
              (#%call-result (new-static-info ... c ...))
              other-static-info ...))])]))
