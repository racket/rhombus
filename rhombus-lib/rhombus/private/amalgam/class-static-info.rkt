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
         "static-info.rkt"
         "class-able.rkt"
         "class-forward-annot.rkt")

(provide (for-syntax extract-instance-static-infoss
                     build-instance-static-infos-defs
                     build-class-static-infos)
         define-constructor-static-info)

(define-for-syntax (extract-instance-static-infoss name-id options super interfaces
                                                   private-interfaces protected-interfaces
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

  (define (get-instance-static-infos internal?)
    #`(#,@(if static-infos-id
              #`((#,(quote-syntax unsyntax-splicing) (syntax-local-value (quote-syntax #,static-infos-id))))
              #'())
       #,@(if super
              (objects-desc-static-infos super)
              #'())
       #,@(apply
           append
           (for/list ([intf (in-list interfaces)]
                      #:unless (and (not internal?)
                                    (or
                                     (hash-ref private-interfaces intf #f)
                                     (hash-ref protected-interfaces intf #f))))
             (syntax->list
              (objects-desc-static-infos intf))))))

  (define instance-static-infos (get-instance-static-infos #f))
  (define internal-instance-static-infos (get-instance-static-infos #t))

  (define common-indirect-static-infos
    #`(#,@(if call-statinfo-indirect-id
              #`((#%indirect-static-info #,call-statinfo-indirect-id))
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

  (define indirect-static-infos
    #`(#,@common-indirect-static-infos
       #,@instance-static-infos))
  (define internal-indirect-static-infos
    #`(#,@common-indirect-static-infos
       #,@internal-instance-static-infos))

  (values call-statinfo-indirect-id
          index-statinfo-indirect-id
          index-set-statinfo-indirect-id
          append-statinfo-indirect-id
          compare-statinfo-indirect-id
          contains-statinfo-indirect-id

          super-call-statinfo-indirect-id

          static-infos-id
          static-infos-exprs
          instance-static-infos

          indirect-static-infos
          internal-indirect-static-infos))

(define-for-syntax (build-instance-static-infos-defs static-infos-id static-infos-exprs)
  (if static-infos-id
      (list
       #`(define-syntax #,static-infos-id
           (#,(quote-syntax quasisyntax)
            (#,@(for/list ([expr (in-list (reverse static-infos-exprs))])
                  #`(#,(quote-syntax unsyntax-splicing) (pack-static-infos 'static_info #,expr)))))))
      null))

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
                       indirect-static-infos
                       dot-providers internal-dot-providers
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
                         [(dep-result ...) dep-results])
             (list
              #'(define-static-info-syntax constructor-name
                  extra ...
                  (#%call-result (dep-result ...
                                             (#%dot-provider dot-providers)
                                             . indirect-static-infos))
                  (#%function-arity arity-mask)
                  . #,(get-function-static-infos)))))
         null)
     (if (and exposed-internal-id
              (syntax-e #'make-internal-name))
         (list
          (with-syntax ([result-infos
                         (let* ([infos #'((#%dot-provider internal-dot-providers))]
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
