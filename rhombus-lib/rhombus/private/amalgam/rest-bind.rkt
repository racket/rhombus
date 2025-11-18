#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt"
                     "origin.rkt")
         (only-in racket/treelist
                  in-treelist)
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt"
         (submod "composite.rkt" for-rest))

(provide (for-spaces (#f
                      rhombus/bind)
                     rest-bind))

(module+ for-function-parse
  (provide (for-syntax extract-rest-info)))

(define-syntax rest-bind
  (expression-prefix-operator
   #f
   `()
   'macro
   (lambda (tail) (error "should not get here"))))

(define-binding-syntax rest-bind
  (binding-prefix-operator
   #f
   `()
   'macro
   (lambda (tail)
     (syntax-parse tail
       ;; used for rest argument in function definition only
       [(_ #:repetition
           rest-arg::binding)
        #:with rest::binding-form #'rest-arg.parsed
        (values
         (transfer-origin
          #'rest
          (binding-form
           #'rest-bind-infoer/repetition
           #'[rest.infoer-id rest.data]))
         #'())]
       [(_ static-infos
           (~optional (~seq #:annot-prefix? annot-prefix?)
                      #:defaults ([annot-prefix? #'#t]))
           rest-arg::binding)
        #:with rest::binding-form #'rest-arg.parsed
        (values
         (transfer-origin
          #'rest
          (binding-form
           #'rest-bind-infoer
           #'[static-infos annot-prefix? rest.infoer-id rest.data]))
         #'())]))))

(define-syntax (rest-bind-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [static-infos annot-prefix? rest-infoer-id rest-data])
     #:with all-static-infos (static-infos-and #'static-infos #'up-static-infos)
     #:with rest-impl::binding-impl #'(rest-infoer-id all-static-infos rest-data)
     (if (syntax-e #'annot-prefix?)
         (annotation-str-update #'rest-impl.info
                                (lambda (str) (string-append "& " str)))
         #'rest-impl.info)]))

(define-for-syntax (annotation-str-update info-stx updater)
  (syntax-parse info-stx
    [info::binding-info
     (binding-info (annotation-string-from-pattern
                    (updater
                     (annotation-string-to-pattern
                      (syntax-e #'info.annotation-str))))
                   #'info.name-id
                   #'info.static-infos
                   #'info.bind-infos
                   #'info.oncer-id
                   #'info.matcher-id
                   #'info.evidence-ids
                   #'info.committer-id
                   #'info.binder-id
                   #'info.data)]))

(define-syntax (rest-bind-infoer/repetition stx)
  (syntax-parse stx
    [(_ _ [rest-infoer-id rest-data])
     #:with rest-impl::binding-impl #'(rest-infoer-id () rest-data)
     #:with rest-info::binding-info #'rest-impl.info
     #:with rest-to-repetition #'in-treelist
     #:with (rest-tmp-id) (generate-temporaries #'(rest-info.name-id))
     #:with rest-seq-tmp-ids (generate-temporaries #'(rest-info.bind-id ...))
     #:with no-rest-map? (free-identifier=? #'always-succeed #'rest-info.matcher-id)
     (binding-info (annotation-string-from-pattern
                    (string-append "["
                                   (annotation-string-to-pattern
                                    (syntax-e #'rest-info.annotation-str))
                                   ", ...]"))
                   #'rest-info.name-id
                   #'rest-info.static-infos
                   (deepen-repetition #'rest-info.bind-infos #'rest-to-repetition (syntax-e #'no-rest-map?))
                   #'rest-bind-oncer/repetition
                   #'rest-bind-matcher/repetition
                   #'rest-tmp-id
                   #'rest-bind-committer/repetition
                   #'rest-bind-binder/repetition
                   #'[rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info])]))

(define-syntax (rest-bind-oncer/repetition stx)
  (syntax-parse stx
    [(_ [rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info::binding-info])
     #'(rest-info.oncer-id rest-info.data)]))

(define-syntax (rest-bind-matcher/repetition stx)
  (syntax-parse stx
    [(_ c-arg-id
        [rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info]
        IF success-expr fail-expr)
     #`(IF #t
           (begin
             (define rest-tmp-id
               #,(make-rest-match #'c-arg-id #'values #'rest-info #'(lambda (arg) #f)
                                  #'rest-to-repetition (syntax-e #'no-rest-map?)))
             (IF rest-tmp-id
                 success-expr
                 fail-expr))
           fail-expr)]))

(define-syntax (rest-bind-committer/repetition stx)
  (syntax-parse stx
    [(_ c-arg-id
        rest-tmp-id/evidence
        [rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info])
     #'(define-values rest-seq-tmp-ids (rest-tmp-id/evidence))]))

(define-syntax (rest-bind-binder/repetition stx)
  (syntax-parse stx
    [(_ c-arg-id
        rest-tmp-id/evidence
        [rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info::binding-info])
     #`(begin
         #,@(make-repetition-bind #'(rest-info.bind-uses ...)
                                  #'(rest-info.bind-id ...)
                                  #'((rest-info.bind-static-info ...) ...)
                                  #'rest-seq-tmp-ids
                                  (syntax-e #'no-rest-map?)
                                  #'rest-to-repetition))]))

(define-for-syntax (extract-rest-info infoer-id info)
  (and (free-identifier=? infoer-id #'rest-bind-infoer/repetition)
       (syntax-parse info
         [info::binding-info
          #:with [rest-to-repetition rest-tmp-id rest-seq-tmp-ids no-rest-map? rest-info] #'info.data
          #'rest-info])))
