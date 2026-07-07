#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "group.rkt")
         "definition.rkt"
         (submod "annotation.rkt" for-class)
         "binding.rkt"
         "dotted-sequence-parse.rkt"
         (submod "equal.rkt" for-parse)
         "parens.rkt"
         "static-info.rkt"
         "indirect-static-info-key.rkt"
         "if-blocked.rkt")

(provide (for-space rhombus/defn
                    annot.def))

(define-defn-syntax annot.def
  (definition-transformer
    (lambda (stx name-prefix effect-id)
      (define (parse form-id lhs rhs)
        (syntax-parse lhs
          [name::dotted-identifier
           (syntax-parse rhs
             [a::annotation
              (syntax-parse #'a.parsed
                [a::annotation-predicate-form
                 #`((define ann-pred a.predicate)
                    (define-static-info-syntax ann-si . a.static-infos)
                    #,(build-syntax-definition/maybe-extension
                       'rhombus/annot #'name.name #'name.extends
                       #'(identifier-annotation ann-pred ((#%indirect-static-info ann-si)))
                       #:form form-id))]
                [a::annotation-binding-form
                 #:with arg-parsed::binding-form #'a.binding
                 #:with arg-impl::binding-impl #`(arg-parsed.infoer-id () arg-parsed.data)
                 #:with arg-info::binding-info #'arg-impl.info
                 #:with ((bind-id bind-use . bind-static-infos) ...) #'arg-info.bind-infos
                 #`((define (ann-converter v)
                      (let* ([tmp-id (let ([arg-info.name-id v])
                                       arg-info.name-id)])
                        (arg-info.oncer-id arg-info.data)
                        (arg-info.matcher-id tmp-id
                                             arg-info.data
                                             if/blocked
                                             (lambda ()
                                               (arg-info.committer-id tmp-id arg-info.evidence-ids arg-info.data)
                                               (arg-info.binder-id tmp-id arg-info.evidence-ids arg-info.data)
                                               (define-static-info-syntax/maybe bind-id . bind-static-infos)
                                               ...
                                               a.body)
                                             #f)))
                    (define-static-info-syntax ann-si . a.static-infos)
                    #,(build-syntax-definition/maybe-extension
                       'rhombus/annot #'name.name #'name.extends
                       #'(identifier-binding-annotation #,(binding-form #'annot-def-infoer
                                                                        #'(ann-converter result ann-si arg-info.matcher-id))
                                                        result
                                                        ((#%indirect-static-info ann-si)))
                       #:form form-id))])])]))
      (syntax-parse stx
        #:datum-literals (op |.|)
        [(form-id (op |.|) . _)
         (raise-syntax-error #f
                             "not defined or imported as a namespace"
                             #'form-id)]
        [(form-id lhs ...+ _::equal rhs ...+)
         (check-multiple-equals stx)
         (parse #'form-id #'(lhs ...) (regroup #'(rhs ...)))]
        [(form-id lhs ...+ (~and blk (_::block rhs-g more-g ...)))
         (unless (null? (attribute more-g))
           (raise-syntax-error #f
                               "expected a block with a single group"
                               stx
                               #'blk))
         (parse #'form-id #'(lhs ...) #'rhs-g)]))))

(define-syntax (annot-def-infoer stx)
  (syntax-parse stx
    [(_ static-infos (converter-id result-id ann-si left-matcher-id))
     (binding-info "annot"
                   #'v
                   (static-infos-and #'static-infos #'((#%indirect-static-info ann-si)))
                   #'((result-id ([#:repet ()])))
                   #'empty-oncer
                   (if (free-identifier=? #'always-succeed #'left-matcher-id)
                       #'always-succeed
                       #'annot-def-matcher)
                   #'thunk
                   #'annot-def-committer
                   #'annot-def-binder
                   #'[thunk converter-id result-id])]))

(define-syntax (annot-def-matcher stx)
  (syntax-parse stx
    [(_ arg-id [thunk converter-id result-id]
        IF success fail)
     #'(begin
         (define thunk (converter-id arg-id))
         (IF thunk success fail))]))

(define-syntax (annot-def-committer stx)
  #'(begin))

(define-syntax (annot-def-binder stx)
  (syntax-parse stx
    [(_ arg-id thunk-id [thunk converter-id result-id])
     #'(define result-id (thunk-id))]))
