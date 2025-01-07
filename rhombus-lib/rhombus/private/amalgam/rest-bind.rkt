#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt")

(provide (for-spaces (#f
                      rhombus/bind)
                     rest-bind))

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
       [(_ static-infos
           (~optional (~seq #:annot-prefix? annot-prefix?)
                      #:defaults ([annot-prefix? #'#t]))
           rest-arg::binding)
        #:with rest::binding-form #'rest-arg.parsed
        (values
         (binding-form
          #'rest-bind-infoer
          #'[static-infos annot-prefix? rest.infoer-id rest.data])
         #'())]))))

(define-syntax (rest-bind-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos [static-infos annot-prefix? rest-infoer-id rest-data])
     #:with all-static-infos (static-infos-union #'static-infos #'up-static-infos)
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
                   #'info.matcher-id
                   #'info.evidence-ids
                   #'info.committer-id
                   #'info.binder-id
                   #'info.data)]))
