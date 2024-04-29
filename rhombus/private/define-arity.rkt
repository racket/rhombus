#lang racket/base
(require racket/stxparam
         (for-syntax racket/base
                     syntax/parse/pre)
         "static-info.rkt"
         "function-arity-key.rkt"
         "indirect-static-info-key.rkt"
         "rhombus-primitive.rkt")

(provide who
         define/arity
         define/method)

(module+ for-info
  (provide indirect-function-static-info
           (for-syntax install-function-static-infos!)))

(define-syntax-parameter who-sym #f)

(define-syntax (who stx)
  (syntax-parse stx
    [_:id
     (define sym (syntax-parameter-value #'who-sym))
     (unless sym
       (raise-syntax-error #f "`who` is unknown" stx))
     #`(quote #,sym)]))

(define-syntax (define/arity stx)
  (expand-define/arity stx build-define/arity))

(define-syntax (define/method stx)
  (syntax-parse stx
    [(self (~seq #:direct-id direct-id) . tail)
     (expand-define/arity #'(self . tail) (build-define/method/direct-id #'direct-id))]
    [_ (expand-define/arity stx build-define/method)]))

(define-for-syntax (expand-define/arity stx build)
  (syntax-parse stx
    [(~or* (~and (_ (~optional (~seq #:name name)) (id . args)
                    (~optional (~and inline #:inline))
                    (~optional (~seq #:primitive (primitive-id ...)))
                    (~optional (~seq #:static-infos static-infos))
                    . body)
                 (~parse rhs #'(lambda args . body)))
           (_ (~optional (~seq #:name name)) id
              (~optional (~and inline #:inline))
              (~optional (~seq #:primitive (primitive-id ...)))
              (~optional (~seq #:static-infos static-infos))
              rhs))
     #`(begin
         #,@(build #'id
                   (attribute name)
                   (and (attribute inline) #t)
                   (if (attribute primitive-id)
                       (syntax->list #'(primitive-id ...))
                       '())
                   (attribute static-infos)
                   #'rhs))]))

(define-for-syntax (build-define/arity id name inline? primitive-ids static-infos rhs [arity-mask #f])
  (define name/id (or name id))
  (define rhs/who
    (with-syntax ([name-sym (syntax-e name/id)])
      (syntax-parse rhs
        #:literals (lambda case-lambda)
        [(lambda ((~seq (~optional kw:keyword) (~or* [id expr] id))
                  ... . rst)
           . body)
         #'(lambda ((~@ (~? kw) (~? [id (syntax-parameterize ([who-sym 'name-sym]) expr)] id))
                    ... . rst)
             (syntax-parameterize ([who-sym 'name-sym])
               . body))]
        [(case-lambda [args . body] ...)
         #'(case-lambda
             [args (syntax-parameterize ([who-sym 'name-sym])
                     . body)]
             ...)])))
  (append
   (for/list ([primitive-id (in-list primitive-ids)])
     #`(set-primitive-who! '#,primitive-id '#,name/id))
   (list (let ([def #`(define #,id
                        #,(if name (syntax-property rhs/who 'inferred-name name) rhs/who))])
           (if inline?
               (syntax-property def 'compiler-hint:cross-module-inline #t)
               def))
         #`(define-static-info-syntax #,id
             #,@(or static-infos '())
             (#%function-arity #,(or arity-mask (extract-arity-mask rhs)))
             (#%indirect-static-info indirect-function-static-info)))))

(define-for-syntax ((build-define/method/direct-id direct-id)
                    id name inline? primitive-ids static-infos rhs)
  (build-define/method id name inline? primitive-ids static-infos rhs
                       #:direct-id direct-id))

(define-for-syntax (build-define/method id name inline? primitive-ids static-infos rhs
                                        #:direct-id [direct-id id])
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
               #`(lambda args
                   #,(make-apply id #'obj #'args)))]
      [(case-lambda [(~or* () (_ . argss) argss) . _] ...)
       (values #'obj
               #`(case-lambda
                   #,@(for/list ([args (in-list (attribute argss))]
                                 #:when args)
                        #`[#,args #,(make-apply id #'obj args)])))]))
  (list* #`(define-for-syntax #,dispatch-id
             (lambda (nary)
               (nary '#,(arithmetic-shift arity-mask -1) (quote-syntax #,direct-id) (quote-syntax #,method-id))))
         #`(define #,method-id
             (lambda (#,obj)
               ;; TODO what should we name the partially applied method?
               ;; This also applies to class methods in general
               #,(syntax-property method 'inferred-name (or name id))))
         (build-define/arity id name inline? primitive-ids static-infos rhs arity-mask)))

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

(define-for-syntax function-static-infos #f)

(define-syntax indirect-function-static-info
  (static-info (lambda () function-static-infos)))

(define-for-syntax (install-function-static-infos! static-infos)
  (set! function-static-infos static-infos))
