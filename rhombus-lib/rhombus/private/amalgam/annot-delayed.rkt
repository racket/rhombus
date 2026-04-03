#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/syntax-local
                     enforest/hier-name-parse
                     "name-path-op.rkt"
                     "srcloc.rkt"
                     "origin.rkt")
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
  (define (make-delayed-annotation get-ids)
    (define-values (pred-id complete!-id static-info-id) (get-ids))
    (delayed-annotation
     #f
     '((default . stronger))
     'macro
     (lambda (stx ctx)
       (syntax-parse stx
         [(head . tail)
          (values (relocate+reraw
                   #'head
                   (annotation-predicate-form
                    pred-id
                    #`((#%indirect-static-info #,static-info-id))))
                  #'tail)]
         [_ 'does-not-happen]))
     complete!-id
     static-info-id
     #f))
  (define (delayed-annotation-ref v) (and (delayed-annotation? v) v)))

(define (too-early who)
  (raise-arguments-error who "delayed annotation is not yet completed"))

(define-for-syntax (make-delayed-static-info who)
  (let ([static-infos #f])
    (static-info
     (case-lambda
       [()
        (or static-infos
            (raise-syntax-error who "annotation static information needed before completed"))]
       [(si)
        (set! static-infos si)]))))

(define-defn-syntax delayed_declare
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (syntax-parse stx
        [(_ name:id)
         #:do [(define (format-id format-str)
                 (datum->syntax #'here (string->symbol (format format-str (syntax-e #'name)))))]
         #:with delayed-predicate (format-id "~a?")
         #:with set-delayed-predicate! (format-id "set-~a?!")
         #:with delayed-static-info (format-id "~a-static-info")
         #`((begin
              (define delayed-predicate (lambda (v) (too-early 'name)))
              (define (set-delayed-predicate! proc) (set! delayed-predicate proc))
              (define-syntax delayed-static-info (make-delayed-static-info 'name))
              (define-syntax #,(in-annotation-space #'name)
                (make-delayed-annotation
                 (lambda ()
                   (values (quote-syntax delayed-predicate)
                           (quote-syntax set-delayed-predicate!)
                           (quote-syntax delayed-static-info)))))))]))))

(define-defn-syntax delayed_complete
  (definition-transformer
    (lambda (stx name-prefix effect-id)
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
            #`(#,(transfer-origins
                  (list (add-origin (in-annotation-space (syntax-local-introduce #'name.name))
                                    #'name.name)
                        #'ap.parsed)
                  #`(begin
                      (define-syntaxes ()
                        (delayed-annotation-complete-compiletime (quote-syntax name.name)
                                                                 (quote-syntax a.static-infos)))
                      (void (#,(delayed-annotation-complete!-id dp) a.predicate)))))]
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
