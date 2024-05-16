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
                  in-static-info-space
                  static-info)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-syntax
          is-simple-pattern?
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

(define-for-syntax (maybe-bind-tail tail-pattern tail)
  (syntax-parse tail-pattern
    [() null]
    [(_ name _)
     (list
      #`(define-syntaxes . #,(make-pattern-variable-bind #'name tail #'unpack-tail-list* 1 '())))]
    [(_ name _ _)
     (list
      #`(define-syntaxes . #,(make-pattern-variable-bind #'name tail #'unpack-tail* 0 '())))]))

;; add tail or not for an `enforest`-based simple pattern
(define-for-syntax (maybe-return-tail expr tail-pattern tail)
  (syntax-parse tail-pattern
    [() #`(values #,expr #,tail)]
    [(~or* (_ _ _) (_ _ _ _)) expr]))

(define-for-syntax (maybe-bind-all all-id self-id make-all-id tail-pattern tail)
  (cond
    [(syntax-e all-id)
     (list
      (syntax-parse tail-pattern
        [() #`(define #,all-id #,self-id)]
        [(~or* (_ _ _) (_ _ _ _)) #`(define #,all-id (#,make-all-id (cons #,self-id #,tail)))])
      #`(define-syntax #,(in-static-info-space all-id) (static-info get-syntax-static-infos)))]
    [else '()]))
