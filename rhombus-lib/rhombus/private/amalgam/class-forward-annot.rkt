#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "static-info.rkt"
         "annotation.rkt"
         (submod "annotation.rkt" for-class)
         (submod "define-arity.rkt" for-info)
         "call-result-key.rkt"
         "values-key.rkt"
         (submod "function-parse.rkt" for-build))

(provide (for-syntax merge-forwards
                     build-forward-annotations))

(define-for-syntax (merge-forwards maybe-ret ret-forwards convert-ok-stx?
                                   #:this? [this? #f])
  (syntax-parse ret-forwards
    [#f (list #'() maybe-ret null)]
    [([(forward-id ...) args (((~var ret (:ret-annotation/prepass (parse-arg-context #:this? this? #'args)))) ...)]
      ...)
     (define (normalize sis)
       (syntax-parse sis
         [(si) #'si]
         [(si ...) #'((#%values (si ...)))]))
     (define static-infos
       (cond
         [(syntax-e convert-ok-stx?) ; => final
          (define static-infoss (map normalize (syntax->list #'((ret.static-infos ...) ...))))
          (for/fold ([static-infos (car static-infoss)]) ([si (in-list (cdr static-infoss))])
            (static-infos-or static-infos si))]
         [else #'()]))
     (define synthesized-annot
       (cond
         [(or (null? static-infos)
              (and (syntax? static-infos) (null? (syntax-e static-infos))))
          #'()]
         [else
          #`((op :~) (parsed #:rhombus/annot #,(annotation-predicate-form #'(lambda xs #t) static-infos)))]))
     (list static-infos
           #`[(parens) #,synthesized-annot]
           #'([forward-id ret.parsed]
              ...
              ...))]))

(define-for-syntax (build-forward-annotations forward-ids-stx forward-c-parsed-stx)
  (for/list ([forward-id (in-list (syntax->list forward-ids-stx))]
             [c-parsed (in-list (syntax->list forward-c-parsed-stx))]
             #:when (syntax-e forward-id))
    (syntax-parse c-parsed
      [c-parsed::annotation-predicate-form
       #`(define-annotation-syntax #,forward-id
           (forwarding-annotation (lambda ()
                                    (annotation-predicate-form
                                     (quote-syntax c-parsed.predicate)
                                     #'()))))]
      [c-parsed::annotation-binding-form
       #`(define-annotation-syntax #,forward-id
           (forwarding-annotation (lambda ()
                                    (annotation-binding-form
                                     (quote-syntax c-parsed.binding)
                                     (quote-syntax c-parsed.body)
                                     #'()))))])))

(define-for-syntax (forwarding-annotation get)
  (annotation-prefix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (stx ctx)
     (values (get)
             #'()))))
