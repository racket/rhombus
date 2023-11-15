#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/hier-name-parse
                     enforest/syntax-local
                     "introducer.rkt"
                     "parse.rkt"
                     "name-path-op.rkt"
                     "name-root.rkt"
                     "define-arity.rkt"
                     "realm.rkt"
                     "pack.rkt")
         "provide.rkt"
         "definition.rkt"
         "name-root.rkt"
         "name-root-ref.rkt"
         "name-root-space.rkt"
         "parens.rkt"
         "parse.rkt"
         (for-syntax "parse.rkt")
         "dotted-sequence-parse.rkt"
         "syntax-parameter.rkt")

(provide (for-space rhombus/namespace
                    syntax_parameter
                    (for-syntax syntax_parameter_meta)))

(begin-for-syntax
  (define in-syntax-parameter-space (make-interned-syntax-introducer/add 'rhombus/syntax_parameter))

  (struct syntax-parameter (key default-val))
  (define (syntax-parameter-ref v) (and (syntax-parameter? v) v)))

(define-name-root syntax_parameter
  #:fields
  (bridge
   relet))

(begin-for-syntax
  (define-name-root syntax_parameter_meta
    #:fields
    (lookup)))

(define-defn-syntax bridge
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        #:datum-literals (group)
        [(form-id name-seq::dotted-identifier-sequence
                  (tag::block body ...))
         #:with name::dotted-identifier #'name-seq
         #`(#,(build-syntax-definition/maybe-extension
               'rhombus/syntax_parameter #'name.name #'name.extends
               #`(syntax-parameter (gensym)
                                   (rhombus-body-at tag body ...))))]))))

(define-syntax relet
  (definition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id name-seq::dotted-identifier-sequence
                 (tag::block body ...))
        #:with (~var name (:hier-name-seq in-name-root-space in-syntax-parameter-space name-path-op name-root-ref)) #'name-seq
        (define p (syntax-local-value* (in-syntax-parameter-space #'name.name) syntax-parameter-ref))
        (unless p (raise-syntax-error #f "not a syntax parameter name" stx #'name-seq))
        #`((define-syntax param-val (rhombus-body-at tag body ...))
           ;; recognized in "forwarding-sequence.rkt":
           (define-syntax-parameter #,(syntax-parameter-key p) param-val))]))))

(begin-for-syntax
  (define/arity (lookup id-in)
    (define id (or (unpack-term id-in #f #f)
                   id-in))
    (unless (identifier? id)
      (raise-argument-error* who rhombus-realm "Identifier" id))
    (define p (syntax-local-value* (in-syntax-parameter-space id) syntax-parameter-ref))
    (unless p
      (raise-arguments-error* who rhombus-realm
                              "identifier is not bound as a syntax parameter"
                              "identifier" id))
    (let loop ([iter (current-syntax-parameters-iterator)])
      (define-values (ht-stx next-iter) (iter))
      (cond
        [(not ht-stx) (syntax-parameter-default-val p)]
        [(hash-ref (syntax-e ht-stx) (syntax-parameter-key p) #f)
         => (lambda (v) (syntax-local-value v))]
        [else (loop next-iter)]))))
