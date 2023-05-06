#lang racket/base
(require (for-syntax racket/base
                     "interface-parse.rkt"
                     "class-parse.rkt"))

(provide (for-syntax callable-statinfo-indirect-id
                     callable-method-status
                     callable-method-as-property
                     callable-method-for-class-desc))

;; get an identifier whose static info describes callable instances as functions;
;; if the relevant `call` method is public or might be privately implemented
;; in this new class, we bind a fresh name to static info, since
;; we haven't parsed enough about the class to predict its implementation's name
(define-for-syntax (callable-statinfo-indirect-id super interfaces name-id intro)
  (or (and (or (and super
                    (memq 'call (class-desc-flags super)))
               (for/or ([intf (in-list interfaces)])
                 (memq 'call (interface-desc-flags intf))))
           ;; => `call` method's infor is relevant
           (intro (datum->syntax #f (string->symbol (format "~a-function-indirect" (syntax-e name-id))))))
      (and super
           ;; => can refer directly to superclass's private implementation, since it can't be overridden here
           (class-desc-call-method-id super))))

;; returns `(values here-callable? public-callable?)`:
;;   * `here-callable?` implies that a `call` method is declared or inherited and implements
;;     `Callable`; if #f, instances may still be callable through an inherited private implementation
;;   * `public-callable?` can be #f even if `here-callable?` if the `call` method is not public
(define-for-syntax (callable-method-status super interfaces method-mindex method-vtable method-private)
  (define call-is-callable?
    (or (and super (memq 'call (class-desc-flags super)))
        (for/or ([intf (in-list interfaces)])
          (memq 'call (interface-desc-flags intf)))))
  (values (and call-is-callable?
               (or (hash-ref method-private 'call #f)
                   (let ([m (hash-ref method-mindex 'call #f)])
                     (and m
                          (not (eq? '#:abstract (vector-ref method-vtable (mindex-index m))))))))
          (and call-is-callable?
               (not (hash-ref method-private 'call #f)))))

(define-for-syntax (callable-method-as-property callable?
                                                method-mindex method-vtable method-private)
  (cond
    [callable?
     (cond
       [(hash-ref method-private 'call #f)
        => (lambda (call-id)
             (list #`(cons prop:procedure #,call-id)))]
       [else
        (define midx (hash-ref method-mindex 'call))
        (list #`(cons prop:procedure
                      #,(vector-ref method-vtable (mindex-index midx))))])]
    [else null]))

(define-for-syntax (callable-method-for-class-desc callable? public-callable?
                                                   super
                                                   method-mindex method-vtable method-private)
  (cond
    [(and callable? (not public-callable?))
     #`(quote-syntax #,(cond
                         [(hash-ref method-private 'call #f)
                          => (lambda (call-id) call-id)]
                         [else
                          (unless super (error "cannot find private call method"))
                          (class-desc-call-method-id super)]))]
    [else #'#f]))
