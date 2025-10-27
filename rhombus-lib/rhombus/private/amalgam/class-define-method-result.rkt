#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt"
                     "class-method-result.rkt"
                     "origin.rkt")
         "static-info.rkt"
         "annotation.rkt"
         (submod "define-arity.rkt" for-info)
         "call-result-key.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         "index-key.rkt"
         "append-key.rkt"
         "compare-key.rkt"
         "contains-key.rkt"
         "values-key.rkt"
         (submod "function-parse.rkt" for-build)
         "class-forward-annot.rkt"
         (only-in "function-arity.rkt"
                  shift-arity))

(provide define-method-result)

(define-syntax (define-method-result stx)
  (syntax-parse stx
    [(_ id
        maybe-ret
        ret-forwards
        (super-result-id ...)
        maybe-id convert-ok? checked-append? checked-compare? kind arity
        maybe-call-statinfo-id
        maybe-ref-statinfo-id+id
        maybe-set-statinfo-id+id
        maybe-append-statinfo-id+id
        maybe-compare-statinfo-id+id
        maybe-contains-statinfo-id+id)
     #:with (_ maybe-ret* ([forward-id forward-c-parsed] ...)) (merge-forwards #'maybe-ret #'ret-forwards #'convert-ok?
                                                                               #:this? #t)
     #:with [args ((~var ret (:ret-annotation (parse-arg-context #:this? #t #'args))))] #'maybe-ret*
     #:do [(define-values (proc predicate? count annot-str static-infos)
             (cond
               [(attribute ret.converter)
                => (lambda (cvtr)
                     (define predicate? (converter-predicate? cvtr))
                     (unless (or predicate?
                                 (syntax-e #'convert-ok?))
                       (raise-syntax-error
                        #f
                        (string-append
                         "declared result annotation must be a predicate annotation;"
                         "\n non-final method cannot have a converter annotation")
                        #'id
                        #'ret))
                     (values (converter-proc cvtr)
                             predicate?
                             (converter-count cvtr)
                             (attribute ret.annot-str)
                             #'ret.static-infos))]
               [else (values #f #t #f #f #'())]))
           (define super-results
             (map syntax-local-method-result
                  (syntax->list #'(super-result-id ...))))
           (define all-count
             (for/foldr ([all-count count])
                        ([r (in-list super-results)]
                         #:do [(define count (method-result-count r))]
                         #:when count)
               ;; TODO improve this error message
               (when (and all-count (not (eqv? count all-count)))
                 (raise-syntax-error
                  #f
                  "incompatible result arities found in the inheritance chain"
                  #'id))
               count))
           (define-values (handler-stx all-annot-str)
             (cond
               [all-count
                (with-syntax ([(val ...) (generate-temporaries
                                          (for/list ([_ (in-range all-count)])
                                            'val))]
                              [(pred ...) (for/list ([r (in-list super-results)]
                                                     #:do [(define pred (method-result-handler-expr r))]
                                                     #:when pred)
                                            pred)])
                  (values (cond
                            [(and predicate?
                                  (null? (syntax-e #'(pred ...)))
                                  (not proc))
                             #f]
                            [predicate?
                             (define all-pred
                               (syntax-parse #'(pred ...)
                                 [(only-pred) #'only-pred]
                                 [_ #'(lambda (val ...)
                                        (and (pred val ...) ...))]))
                             (if proc
                                 #`(lambda (val ...)
                                     (#,proc val ... #,all-pred (lambda () #f)))
                                 all-pred)]
                            ;; converter case: `proc` must be non-`#f`
                            [else
                             (if (null? (syntax-e #'(pred ...)))
                                 proc
                                 #`(lambda (val ... success-k fail-k)
                                     (#,proc
                                      val ...
                                      (lambda (val ...)
                                        (if (and (pred val ...) ...)
                                            (success-k val ...)
                                            (fail-k)))
                                      fail-k)))])
                          (for/foldr ([all-annot-str annotation-any-string])
                                     ([annot-str (in-list (cons annot-str
                                                                (map method-result-annot-str super-results)))]
                                      #:when annot-str)
                            (annotation-string-and annot-str all-annot-str))))]
               ;; no annotation in the inheritance chain
               [else (values #f #f)]))
           ;; Like `Super_1 && ... && Super_n && This`, in reverse of
           ;; the actual checks (checks happen "bottom-up"); or in
           ;; other words, a variant of `&&` that prioritizes the
           ;; left-hand side static infos.
           (define all-static-infos
             (if all-count
                 (for/foldr ([all-static-infoss (for/list ([_ (in-range all-count)])
                                                  #'())]
                             #:result (if (eqv? all-count 1)
                                          #`#,(car all-static-infoss)
                                          #`((#%values #,all-static-infoss))))
                            ([infos (in-list (append (list static-infos)
                                                     (map method-result-static-infos super-results)))])
                   (for/list ([infos (in-list (normalize-static-infos/values all-count infos))]
                              [all-static-infos (in-list all-static-infoss)])
                     (static-infos-and infos all-static-infos)))
                  #'()))]
     #:attr handler handler-stx
     #:attr handler-id (and handler-stx
                            (not (identifier? handler-stx))
                            ((make-syntax-introducer)
                             (datum->syntax #f
                                            (string->symbol
                                             (format "~a-result-handler" (syntax-e #'id))))))
     (define (gen id [de-method? #f])
       (if (syntax-e id)
           (list (with-syntax ([id id]
                               [result-infos
                                (cond
                                  [(static-infos-empty? all-static-infos) #'()]
                                  [(eq? (syntax-e #'kind) 'property)
                                   #`(#:at_arities ((#,(arithmetic-shift 1 (if de-method? 0 1)) #,all-static-infos)))]
                                  [else all-static-infos])]
                               [(maybe-arity-info ...)
                                (if (syntax-e #'arity)
                                    (list #`(#%function-arity #,(if de-method?
                                                                    (de-method-arity #'arity)
                                                                    #'arity)))
                                    '())])
                   (with-syntax ([(maybe-call-result ...)
                                  (if (static-infos-empty? #'result-infos)
                                      null
                                      (list #'(#%call-result result-infos)))])
                     #'(define-static-info-syntax id
                         maybe-call-result ...
                         maybe-arity-info ...
                         . #,(indirect-get-function-static-infos)))))
           '()))
     (define (gen-bounce ind-id+id key result-key #:box-id? [box-id? #f])
       (if (syntax-e ind-id+id)
           (syntax-parse ind-id+id
             [(ind-id id)
              (list #`(define-static-info-syntax ind-id
                        (#,key #,(if box-id?
                                     (box-immutable #'id)
                                     #'id))
                        #,@(if result-key
                               (list #`(#,result-key #,all-static-infos))
                               '())))])
           '()))
     (transfer-origins
      (attribute ret.origins)
      #`(begin
          ;; Each `forward-id` annotation is used in an expression that
          ;; implements a method, needed to apply potential checks/conversions
          #,@(build-forward-annotations #'(forward-id ...)
                                        #'(forward-c-parsed ...))
          (~? (define handler-id handler))
          #,@(if (syntax-e #'id)
                 (list
                  #`(define-syntax id
                      (method-result-maker
                       (lambda ()
                         (method-result (~? (quote-syntax handler-id) (~? (quote-syntax handler) #f))
                                        (quote #,predicate?)
                                        (quote #,all-count)
                                        (quote #,all-annot-str)
                                        (quote-syntax #,all-static-infos)
                                        (quote arity))))))
                 null)
          #,@(gen #'maybe-id)
          #,@(gen #'maybe-call-statinfo-id #t)
          #,@(gen-bounce #'maybe-ref-statinfo-id+id #'#%index-get #'#%index-result)
          #,@(gen-bounce #'maybe-set-statinfo-id+id #'#%index-set #f)
          #,@(gen-bounce #'maybe-append-statinfo-id+id #'#%append #f
                         ;; boxed identifier means "checked" for `#%append`
                         #:box-id? (syntax-e #'checked-append?))
          #,@(gen-bounce #'maybe-compare-statinfo-id+id #'#%compare #f
                         #:box-id? (syntax-e #'checked-compare?))
          #,@(gen-bounce #'maybe-contains-statinfo-id+id #'#%contains #f)))]))

(define-for-syntax (de-method-arity arity)
  (datum->syntax #f
                 (shift-arity
                  (syntax-parse arity
                    [(n required-kws allowed-kws)
                     (list (syntax-e #'n) #'required-kws #'allowed-kws)]
                    [n
                     (syntax-e #'n)])
                  -1)))
