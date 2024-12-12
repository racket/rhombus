#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parens.rkt"
         "unquote-binding.rkt"
         (only-in "unquote-binding-primitive.rkt"
                  #%parens)
         "op-literal.rkt"
         "pattern-variable.rkt"
         "pack.rkt"
         (only-in "static-info.rkt"
                  define-static-info-syntax)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax
          is-simple-pattern?
          generate-simple-pattern-check
          maybe-bind-tail
          maybe-return-tail
          maybe-bind-all))

(define-for-syntax (is-simple-pattern? tail-pattern)
  (syntax-parse tail-pattern
    [() #t]
    [(_::$-bind name:identifier _::...-bind) #t]
    [(_::$-bind name:identifier _::$-bind (tag::parens))
     (free-identifier=? (in-unquote-binding-space (datum->syntax #'tag '#%parens))
                        (unquote-bind-quote #%parens))]
    [_ #f]))

(define-for-syntax (generate-simple-pattern-check self-id tail-pattern tail-id)
  (syntax-parse tail-pattern
    [() #`(unless (null? (syntax-e (unpack-tail #,tail-id #f #f))) (unexpected-term #,self-id #,tail-id))]
    ;; other simple patterns always match:
    [_ #'(void)]))

(define (unexpected-term self-id tail)
  (define t (syntax-e (unpack-tail tail #f #f)))
  (raise-syntax-error #f
                      "unexpected term"
                      (datum->syntax #f (cons self-id t))
                      (car t)))

(define-for-syntax (maybe-bind-tail tail-pattern tail)
  (syntax-parse tail-pattern
    [() null]
    [(_ name _)
     (with-syntax ([(ids rhs . _) (make-pattern-variable-bind #'name tail #'unpack-tail-list* 1)])
       (list
        #`(define-syntaxes ids rhs)))]
    [(_ name _ _)
     (with-syntax ([(ids rhs . _) (make-pattern-variable-bind #'name tail #'unpack-tail* 0)])
       (list
        #`(define-syntaxes ids rhs)))]))

;; add tail or not for an `enforest`-based simple pattern
(define-for-syntax (maybe-return-tail expr tail-pattern tail)
  (syntax-parse tail-pattern
    [() #`(values #,expr #,tail)]
    [(~or* (_ _ _) (_ _ _ _)) expr]))

(define-for-syntax (maybe-bind-all all-id self-id make-all-id tail-pattern tail)
  (cond
    [(syntax-e all-id)
     (list
      #`(define #,all-id
          #,(syntax-parse tail-pattern
              [() self-id]
              [(~or* (_ _ _) (_ _ _ _)) #`(#,make-all-id (cons #,self-id #,tail))]))
      #`(define-static-info-syntax #,all-id #:getter get-syntax-static-infos))]
    [else '()]))
