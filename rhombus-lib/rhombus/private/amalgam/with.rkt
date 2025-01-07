#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     "statically-str.rkt")
         "expression.rkt"
         "static-info.rkt"
         "parse.rkt"
         "dot-provider-key.rkt"
         "reconstructor.rkt"
         "parens.rkt"
         "realm.rkt"
         (submod "equal.rkt" for-parse)
         "order.rkt"
         "order-primitive.rkt"
         "is-static.rkt")

(provide with)

(module+ for-update
  (provide define-update-syntax
           (for-syntax update-transformer)))

(begin-for-syntax
  (define in-update-space (make-interned-syntax-introducer 'rhombus/update))

  (struct update-transformer (proc))
  (define (update-transformer-ref v)
    (and (update-transformer? v) v))

  (define-syntax-class :update-provider
    (pattern (~var ref-id (:static-info #'#%dot-provider))
             #:with id #'ref-id.val)))

(define-syntax with
  (expression-infix-operator
   #f
   (lambda ()
     `((,(order-quote assignment) . stronger)
       (default . weaker)))
   'macro
   (lambda (orig-form1 tail)
     (define more-static? (is-static-context/tail? tail))
     (syntax-parse tail
       #:datum-literals (group)
       [(with-id (_::parens (group name:id _::equal rhs ...) ...) . tail)
        (let ([form1 (rhombus-local-expand orig-form1)])
          (define update-id/s
            (syntax-parse form1
              [dp::update-provider #'dp.id]
              [_ #f]))
          (define update-ids (extract-dot-provider-ids update-id/s))
          (define updater
            (and (pair? update-ids)
                 (null? (cdr update-ids))
                 (syntax-local-value* (in-update-space (car update-ids)) update-transformer-ref)))
          (when (and more-static? (not updater))
            (raise-syntax-error #f
                                (string-append "no update implementation available" statically-str)
                                #'with-id
                                (unwrap-static-infos orig-form1)))
          (for/fold ([seen #hasheq()]) ([name (in-list (syntax->list #'(name ...)))])
            (when (hash-ref seen (syntax-e name) #f)
              (raise-syntax-error #f "duplicate field for update" #'with-id name))
            (hash-set seen (syntax-e name) #t))
          (values
           (or (and updater
                    ((update-transformer-proc updater) form1 #'with-id
                                                       (syntax->list #'(name ...))
                                                       (syntax->list #'((rhombus-expression (group rhs ...)) ...))
                                                       more-static?))
               (let ([name-map (for/hasheq ([name (in-list (syntax->list #'(name ...)))]
                                            [pos (in-naturals)])
                                 (values (syntax-e name) pos))])
                 #`(dynamic-update 'with-id #,form1 '#,name-map (vector (rhombus-expression (group rhs ...)) ...))))
           #'tail))]
       [(with-id (_::parens g ...) . tail)
        (for ([g (in-list (syntax->list #'(g ...)))])
          (syntax-parse g
            #:datum-literals (group)
            [(group field-name:id _::equal rhs ...) (void)]))]
       [(with-id . _)
        (raise-syntax-error #f "expected parentheses afterward" #'with-id)]))
   'left))

(define (dynamic-update who obj field-map field-vals)
  (define r (reconstructor-ref obj #f))
  (unless r
    (raise-arguments-error* who rhombus-realm
                            "value does not support functional update"
                            "value" obj))
  (define arg-sym+accs (car r))
  (define recon-proc (cdr r))
  (define-values (supplied-c args)
    (for/fold ([supplied-c 0] [args '()])
              ([arg-sym+acc (in-list (reverse arg-sym+accs))])
      (define i (hash-ref field-map (car arg-sym+acc) #f))
      (if i
          (values (add1 supplied-c)
                  (cons (vector-ref field-vals i) args))
          (values supplied-c
                  (cons ((cdr arg-sym+acc) obj) args)))))
  (unless (= supplied-c (hash-count field-map))
    (for ([k (in-hash-keys field-map)])
      (unless (assq k arg-sym+accs)
        (raise-arguments-error* who rhombus-realm
                                "no such reconstructor argument in class"
                                "name" (datum->syntax #f k)))))
  (apply recon-proc obj args))

(define-syntax (define-update-syntax stx)
  (syntax-parse stx
    [(_ id rhs)
     #`(define-syntax #,(in-update-space #'id) rhs)]))
