#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "name-path-op.rkt")
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "parens.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "dotted-sequence-parse.rkt"
         "static-info.rkt"
         "indirect-static-info-key.rkt")

(provide (for-space rhombus/defn
                    delayed_declare
                    delayed_complete))

(begin-for-syntax
  (struct delayed-annotation annotation-prefix-operator (complete!-id
                                                         static-info-id
                                                         [completed? #:mutable]))
  (define (make-delayed-annotation proc complete!-id static-info-id)
    (delayed-annotation
     '((default . stronger))
     'macro
     proc
     complete!-id
     static-info-id
     #f))
  (define (delayed-annotation-ref v) (and (delayed-annotation? v) v)))

(define (too-early who)
  (raise-arguments-error who "delayed annotation is not yet completed"))

(define-for-syntax (static-info-too-early who)
  (raise-syntax-error who
                      "annotation static information needed before completed"))

(define-defn-syntax delayed_declare
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        [(_ name:id)
         #`((begin
              (define delayed-predicate (lambda (v) (too-early 'name)))
              (define (set-delayed-predicate! proc) (set! delayed-predicate proc))
              (define-syntax delayed-static-info (static-info
                                                  (let ([static-infos #f])
                                                    (case-lambda
                                                      [()
                                                       (unless static-infos (static-info-too-early 'name))
                                                       static-infos]
                                                      [(si) (set! static-infos si)]))))
              (define-syntax #,(in-annotation-space #'name)
                (letrec ([self (make-delayed-annotation
                                (lambda (stx)
                                  (values (annotation-predicate-form
                                           #'delayed-predicate
                                           #'((#%indirect-static-info delayed-static-info)))
                                          (syntax-parse stx
                                            [(_ . tail) #'tail]
                                            [_ 'does-not-happen])))
                                #'set-delayed-predicate!
                                #'delayed-static-info)])
                  self))))]))))

(define-defn-syntax delayed_complete
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        [(_ name-seq::dotted-identifier-sequence (_::block g))
         #:with (~var name (:hier-name-seq in-name-root-space in-annotation-space name-path-op name-root-ref)) #'name-seq
         #:do [(unless (null? (syntax-e #'name.tail))
                 (raise-syntax-error #f
                                     "not a delayed annotation name"
                                     stx
                                     #'name-seq))]
         #:with ap::annotation #'g
         (define dp (syntax-local-value* (in-annotation-space #'name.name) delayed-annotation-ref))
         (unless dp
           (raise-syntax-error #f
                               "not defined as a delayed annotation"
                               stx
                               #'name.name))
         (syntax-parse #'ap.parsed
           [a::annotation-predicate-form
            #`((define-syntaxes ()
                 (delayed-annotation-complete-compiletime #'name.name #'a.static-infos))
               (#,(delayed-annotation-complete!-id dp) a.predicate))]
           [a::annotation-binding-form
            (raise-syntax-error #f
                                "supported only for predicate-based annotations"
                                stx)])]))))

(define-for-syntax (delayed-annotation-complete-compiletime name static-infos)
  (define dp (syntax-local-value* (in-annotation-space name) delayed-annotation-ref))
  (unless dp
    ;; should not happen:
    (raise-syntax-error #f "not a delayed annotation" name))
  (when (delayed-annotation-completed? dp)
    (raise-syntax-error #f
                        "delayed annotation is already completed"
                        name))
  (define si (syntax-local-value* (delayed-annotation-static-info-id dp) static-info-ref))
  (unless si
    ;; should not happen:
    (raise-syntax-error #f "static info not found" name))
  ((static-info-get-stxs si) (syntax->list static-infos))
  (set-delayed-annotation-completed?! dp #t)
  (values))
