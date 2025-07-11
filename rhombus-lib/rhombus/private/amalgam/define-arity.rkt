#lang racket/base
(require (for-syntax racket/base
                     racket/private/immediate-default
                     syntax/parse/pre)
         racket/stxparam
         "static-info.rkt"
         "function-arity-key.rkt"
         "rhombus-primitive.rkt"
         "error-adjust.rkt"
         "function-arity.rkt")

(provide who
         define/arity
         define/method)

(module+ for-info
  (provide (for-syntax (rename-out [get-function-static-infos indirect-get-function-static-infos])
                       install-get-function-static-infos!)))

(define-syntax-parameter who-sym #f)

(define-syntax (who stx)
  (syntax-parse stx
    [_:id
     (define sym (syntax-parameter-value #'who-sym))
     (unless sym
       (raise-syntax-error #f "`who` is unknown" stx))
     #`(quote #,sym)]))

(define-syntax (with-who stx)
  (syntax-parse stx
    [(_ who . body)
     #'(syntax-parameterize ([who-sym 'who])
         . body)]))

(define-syntax (define/arity stx)
  (expand-define/arity stx build-define/arity))

(define-syntax (define/method stx)
  (syntax-parse stx
    [(self (~seq #:direct-id direct-id) . tail)
     (expand-define/arity #'(self . tail)
                          (lambda (id name primitive-ids local-primitive-ids static-infos rhs)
                            (build-define/method id #'direct-id name primitive-ids local-primitive-ids static-infos rhs)))]
    [_
     (expand-define/arity stx
                          (lambda (id name primitive-ids local-primitive-ids static-infos rhs)
                            (build-define/method id id name primitive-ids local-primitive-ids static-infos rhs)))]))

(define-for-syntax (expand-define/arity stx build)
  (syntax-parse stx
    [(~or* (~and (_ (~optional (~seq #:name name)) (id . args)
                    (~optional (~seq #:primitive (primitive-id ...)))
                    (~optional (~seq #:local-primitive (local-primitive-id ...)))
                    (~optional (~seq #:static-infos static-infos))
                    . body)
                 (~parse rhs (syntax/loc stx (lambda args . body))))
           (_ (~optional (~seq #:name name)) id
              (~optional (~seq #:primitive (primitive-id ...)))
              (~optional (~seq #:local-primitive (local-primitive-id ...)))
              (~optional (~seq #:static-infos static-infos))
              rhs))
     #`(begin
         #,@(build #'id
                   (attribute name)
                   (if (attribute primitive-id)
                       (syntax->list #'(primitive-id ...))
                       '())
                   (if (attribute local-primitive-id)
                       (syntax->list #'(local-primitive-id ...))
                       '())
                   (attribute static-infos)
                   #'rhs))]))

(define-for-syntax (build-define/arity id name primitive-ids local-primitive-ids static-infos rhs [arity-mask #f])
  (define name/id (or name id))
  (define rhs/who
    (with-syntax ([(local-primitive ...) local-primitive-ids]
                  [name/id name/id])
      (syntax-parse rhs
        #:literals (lambda case-lambda)
        [(lambda ((~seq (~optional kw:keyword)
                        (~or* (~and [id expr]
                                    (~parse wrapped-expr
                                            ;; duplicate a check here
                                            (if (immediate-default? #'expr)
                                                #'expr
                                                #'(with-who name/id expr))))
                              id))
                  ... . rst)
           . body)
         (syntax/loc rhs
           (lambda ((~@ (~? kw) (~? [id wrapped-expr] id))
                    ... . rst)
             (with-error-adjust-primitive ([local-primitive name/id] ...)
               (with-who name/id . body))))]
        [(case-lambda
           [args . body]
           ...)
         (syntax/loc rhs
           (case-lambda
             [args
              (with-error-adjust-primitive ([local-primitive name/id] ...)
                (with-who name/id . body))]
             ...))])))
  (append
   (for/list ([primitive-id (in-list primitive-ids)])
     #`(void (set-primitive-who! '#,primitive-id '#,name/id)))
   (list #`(define #,id
             #,(if name (syntax-property rhs/who 'inferred-name name) rhs/who))
         (with-syntax ([id id]
                       [(info ...) (or static-infos '())]
                       [arity-mask (or arity-mask (extract-arity-mask rhs))])
           #'(define-static-info-syntax id
               info ...
               (#%function-arity arity-mask)
               . #,(get-function-static-infos))))))

(define-for-syntax (build-define/method id direct-id name primitive-ids local-primitive-ids static-infos rhs)
  (define (format-id fmt)
    (datum->syntax id (string->symbol (format fmt (syntax-e id)))))
  (define dispatch-id (format-id "~a/dispatch"))
  (define method-id (format-id "~a/method"))
  (define arity-mask (extract-arity-mask rhs))
  (define-values (obj method)
    (syntax-parse rhs
      #:literals (lambda case-lambda)
      [(lambda (~or* ((~or* [obj . _] obj) . args)
                     (~and args (~parse obj #'obj)))
         . _)
       (values #'obj
               (quasisyntax/loc rhs
                 (lambda args
                   #,(make-apply id #'obj #'args))))]
      [(case-lambda [(~or* () (_ . argss) argss) . _] ...)
       (values #'obj
               (quasisyntax/loc rhs
                 (case-lambda
                   #,@(for/list ([args (in-list (attribute argss))]
                                 #:when args)
                        #`[#,args #,(make-apply id #'obj args)]))))]))
  (list* #`(define-for-syntax #,dispatch-id
             (lambda (nary)
               (nary '#,(shift-arity arity-mask -1) (quote-syntax #,direct-id) (quote-syntax #,method-id))))
         #`(define #,method-id
             (lambda (#,obj)
               #,(syntax-property method 'inferred-name (or name id))))
         (build-define/arity id name primitive-ids local-primitive-ids static-infos rhs arity-mask)))

(define-for-syntax (make-apply rator obj rands)
  (define (extract-arg arg)
    (syntax-parse arg
      [[id . _] #'id]
      [_ arg]))
  (syntax-parse rands
    [(_ ...)
     #`(#,rator #,obj #,@(map extract-arg (syntax->list rands)))]
    [(arg ... . rest-arg)
     #`(apply #,rator #,obj #,@(map extract-arg (syntax->list #'(arg ...))) rest-arg)]))

(define-for-syntax (extract-arity-mask rhs)
  (syntax-parse rhs
    #:literals (lambda case-lambda)
    [(lambda args . _)
     (extract-arity #'args)]
    [(case-lambda [args . _] ...)
     (apply bitwise-ior (map extract-arity (syntax->list #'(args ...))))]))

(define-for-syntax (extract-arity args)
  (let loop ([args args] [mask 1] [allowed-kws '()] [req-kws '()])
    (syntax-parse args
      [() (if (null? allowed-kws)
              mask
              `(,mask ,(sort req-kws keyword<?) ,(sort allowed-kws keyword<?)))]
      [(_:identifier . args) (loop #'args (arithmetic-shift mask 1) allowed-kws req-kws)]
      [([_:identifier _] . args)
       (define r (loop #'args (arithmetic-shift mask 1) allowed-kws req-kws))
       (if (integer? r)
           (bitwise-ior mask r)
           (cons (bitwise-ior mask (car r)) (cdr r)))]
      [_:identifier (loop #'() (bitwise-not (sub1 (arithmetic-shift mask 1))) allowed-kws req-kws)]
      [(kw:keyword _:identifier . args) (loop #'args mask
                                              (cons (syntax-e #'kw) allowed-kws)
                                              (cons (syntax-e #'kw) req-kws))]
      [(kw:keyword [_:identifier _] . args) (loop #'args mask
                                                  (cons (syntax-e #'kw) allowed-kws)
                                                  req-kws)])))

(define-for-syntax get-function-static-infos #f)

(define-for-syntax (install-get-function-static-infos! get-static-infos)
  (set! get-function-static-infos get-static-infos))
