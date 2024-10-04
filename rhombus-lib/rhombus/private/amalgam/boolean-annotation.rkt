#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt"
                     "srcloc.rkt")
         (submod "annotation.rkt" for-class)
         "binding.rkt"
         "static-info.rkt"
         "if-blocked.rkt")

(provide (for-space rhombus/annot
                    &&
                    \|\|
                    !))

;; ----------------------------------------
;; &&

(define-annotation-syntax &&
  (annotation-infix-operator
   (lambda () `((,(annot-quote \|\|) . stronger)))
   'automatic
   (lambda (lhs rhs stx)
     (relocate+reraw
      (datum->syntax #f (list lhs stx rhs))
      (syntax-parse (list lhs rhs)
        [(l::annotation-predicate-form r::annotation-predicate-form)
         (annotation-predicate-form
          #`(let ([l-pred l.predicate]
                  [r-pred r.predicate])
              (lambda (v)
                (and (l-pred v) (r-pred v))))
          (static-infos-union #'r.static-infos #'l.static-infos))]
        [(l::annotation-binding-form r::annotation-binding-form)
         (annotation-binding-form
          (binding-form
           #'and-infoer
           #`[result l.binding r.binding l.body r.body r.static-infos])
          #'result
          #'r.static-infos)])))
   'left))

(define-syntax (and-infoer stx)
  (syntax-parse stx
    [(_ static-infos (right-val lhs-i::binding-form rhs-i::binding-form lhs-body rhs-body rhs-static-infos))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with rhs-impl::binding-impl #`(rhs-i.infoer-id #,(static-infos-union #'lhs.static-infos #'static-infos) rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with (lhs-bind-info ...) #'lhs.bind-infos
     (binding-info (annotation-string-and (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   #'rhs.static-infos ; presumably includes `lhs.static-infos` as passed to `rhs-i.infoer-id`
                   #'((right-val (0) . rhs-static-infos))
                   #'and-matcher
                   #'and-committer
                   #'and-binder
                   #'(left-val right-val lhs rhs lhs-body rhs-body))]))

(define-syntax (and-matcher stx)
  (syntax-parse stx
    [(_ arg-id (left-id bind-id lhs::binding-info rhs::binding-info lhs-body rhs-body) IF success fail)
     #:with ((lhs-bind-id bind-use . lhs-bind-static-infos) ...) #'lhs.bind-infos
     #'(lhs.matcher-id arg-id lhs.data IF
                       (begin
                         (lhs.committer-id arg-id lhs.data)
                         (lhs.binder-id arg-id lhs.data)
                         (define-static-info-syntax/maybe lhs-bind-id . lhs-bind-static-infos)
                         ...
                         (define left-id lhs-body)
                         (rhs.matcher-id left-id rhs.data IF success fail))
                       fail)]))

(define-syntax (and-committer stx)
  (syntax-parse stx
    [(_ arg-id (left-id bind-id lhs::binding-info rhs::binding-info lhs-body rhs-body))
     #'(rhs.committer-id left-id rhs.data)]))

(define-syntax (and-binder stx)
  (syntax-parse stx
    [(_ arg-id (left-id bind-id lhs::binding-info rhs::binding-info lhs-body rhs-body))
     #:with ((rhs-bind-id bind-use . rhs-bind-static-infos) ...) #'rhs.bind-infos
     #`(begin
         (rhs.binder-id left-id rhs.data)
         (define-static-info-syntax/maybe rhs-bind-id . rhs-bind-static-infos)
         ...
         (define bind-id rhs-body))]))

;; ----------------------------------------
;; ||

(define-annotation-syntax \|\|
  (annotation-infix-operator
   null
   'automatic
   (lambda (lhs rhs stx)
     (relocate+reraw
      (datum->syntax #f (list lhs stx rhs))
      (syntax-parse (list lhs rhs)
        [(l::annotation-predicate-form r::annotation-predicate-form)
         (annotation-predicate-form
          #'(let ([l-pred l.predicate]
                  [r-pred r.predicate])
              (lambda (v)
                (or (l-pred v) (r-pred v))))
          (static-infos-intersect #'l.static-infos #'r.static-infos))]
        [(l::annotation-binding-form r::annotation-binding-form)
         (annotation-binding-form
          (binding-form
           #'or-infoer
           #'[result l.binding l.body l.static-infos r.binding r.body r.static-infos])
          #'result
          (static-infos-intersect #'l.static-infos #'r.static-infos))])))
   'left))

(define-syntax (or-infoer stx)
  (syntax-parse stx
    [(_ static-infos (bind-id lhs-i::binding-form lhs-body lhs-static-infos rhs-i::binding-form rhs-body rhs-static-infos))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id static-infos rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with result-static-infos (static-infos-intersect #'lhs-static-infos #'rhs-static-infos)
     (binding-info (annotation-string-or (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   (static-infos-intersect #'lhs.static-infos #'rhs.static-infos)
                   #`((bind-id (0) . result-static-infos))
                   #'or-matcher
                   #'or-committer
                   #'or-binder
                   #'(lhs rhs finish bind-id lhs-body rhs-body lhs.bind-infos rhs.bind-infos))]))

(define-syntax (or-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info finish-id result-id left-body right-body
                                  ((lhs-bind-id lhs-bind-use . lhs-bind-static-infos) ...)
                                  ((rhs-bind-id rhs-bind-use . rhs-bind-static-infos) ...))
        IF success fail)
     #`(begin
         (define finish-id
           ;; preserve the textual order
           ((lambda (right-k)
              (lhs.matcher-id arg-id lhs.data if/blocked
                              (lambda ()
                                (lhs.committer-id arg-id lhs.data)
                                (lhs.binder-id arg-id lhs.data)
                                (define-static-info-syntax/maybe lhs-bind-id . lhs-bind-static-infos)
                                ...
                                left-body)
                              (right-k)))
            (lambda ()
              (rhs.matcher-id arg-id rhs.data if/blocked
                              (lambda ()
                                (rhs.committer-id arg-id rhs.data)
                                (rhs.binder-id arg-id rhs.data)
                                (define-static-info-syntax/maybe rhs-bind-id . rhs-bind-static-infos)
                                ...
                                right-body)
                              #f))))
         (IF finish-id success fail))]))

(define-syntax (or-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id . _))
     #'(begin)]))

(define-syntax (or-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id result-id . _))
     #'(define result-id (finish-id))]))

;; ----------------------------------------
;; !

(define-annotation-syntax !
  (annotation-prefix-operator
   (lambda () `((,(annot-quote &&) . stronger)
                (,(annot-quote \|\|) . stronger)))
   'automatic
   (lambda (form stx)
     (relocate+reraw
      (datum->syntax #f (list stx form))
      (syntax-parse form
        [f::annotation-predicate-form
         (annotation-predicate-form
          #'(let ([pred f.predicate])
              (lambda (v)
                (not (pred v))))
          #'())]
        [f::annotation-binding-form
         (annotation-binding-form
          (binding-form
           #'not-infoer
           #'[result f.binding])
          #'result
          #'())])))))

(define-syntax (not-infoer stx)
  (syntax-parse stx
    [(_ static-infos [result-id form::binding-form])
     #:with impl::binding-impl #'(form.infoer-id () form.data)
     #:with info::binding-info #'impl.info
     (binding-info (string-append "!" (syntax-e #'info.annotation-str))
                   #'info.name-id
                   #'static-infos
                   #'()
                   #'not-matcher
                   #'not-committer
                   #'not-binder
                   #'[result-id info])]))

(define-syntax (not-matcher stx)
  (syntax-parse stx
    [(_ arg-id [result-id info::binding-info]
        IF success fail)
     #'(IF (let ()
             (info.matcher-id arg-id info.data if/blocked
                              #f
                              #t))
           success
           fail)]))

(define-syntax (not-committer stx)
  (syntax-parse stx
    [(_ arg-id [result-id info])
     #'(begin)]))

(define-syntax (not-binder stx)
  (syntax-parse stx
    [(_ arg-id [result-id info])
     #'(define result-id arg-id)]))
