#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "binding.rkt"
         "parse.rkt"
         "static-info.rkt")

(provide (for-space rhombus/bind
                    &&
                    \|\|))

(module+ for-class
  (provide (for-syntax make-and-binding)))

;; ----------------------------------------
;; &&

(define-binding-syntax &&
  (binding-infix-operator
   (bind-quote &&)
   (list (cons (bind-quote \|\|) 'stronger))
   'automatic
   (lambda (lhs rhs stx)
     (binding-form
      #'and-infoer
      #`(#,lhs #,rhs)))
   'left))

(define-for-syntax (make-and-binding lhs rhs)
  (binding-form #'and-infoer
                #`(#,lhs #,rhs)))
  
(define-syntax (and-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with rhs-impl::binding-impl #`(rhs-i.infoer-id #,(static-infos-union #'lhs.static-infos #'static-infos) rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with (lhs-bind-info ...) #'lhs.bind-infos
     (binding-info (annotation-string-and (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   #'rhs.static-infos ; presumably includes `lhs.static-infos` as passed to `rhs-id.infoer-id`
                   #'(lhs-bind-info ... . rhs.bind-infos)
                   #'and-matcher
                   #'and-committer
                   #'and-binder
                   #'(lhs rhs))]))

(define-syntax (and-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info) IF success fail)
     #'(lhs.matcher-id arg-id lhs.data IF
                       (rhs.matcher-id arg-id rhs.data IF success fail)
                       fail)]))

(define-syntax (and-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info))
     #'(begin
         (lhs.committer-id arg-id lhs.data)
         (rhs.committer-id arg-id rhs.data))]))

(define-syntax (and-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info))
     #`(begin
         (lhs.binder-id arg-id lhs.data)
         (rhs.binder-id arg-id rhs.data))]))

;; ----------------------------------------
;; ||

(define-binding-syntax \|\|
  (binding-infix-operator
   (bind-quote \|\|)
   null
   'automatic
   (lambda (lhs rhs stx)
     (binding-form
      #'or-infoer
      #`(#,lhs #,rhs)))
   'left))

(define-syntax (or-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id static-infos rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     (binding-info (annotation-string-or (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   (static-infos-intersect #'lhs.static-infos #'rhs.static-infos)
                   #'()
                   #'or-matcher
                   #'or-committer
                   #'or-binder
                   #'(lhs rhs finish))]))

(define-syntax (or-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info finish-id)
        IF success fail)
     #`(begin
         (define finish-id
           (let ()
             (lhs.matcher-id arg-id lhs.data block-if
                             (lambda ()
                               (lhs.committer-id arg-id lhs.data)
                               (lhs.binder-id arg-id lhs.data)
                               (void))
                             (rhs.matcher-id arg-id rhs.data block-if
                                             (lambda ()
                                               (rhs.committer-id arg-id rhs.data)
                                               (rhs.binder-id arg-id rhs.data)
                                               (void))
                                             #f))))
         (IF finish-id success fail))]))

(define-syntax-rule (block-if a b c)
  (if (let () a)
      (let () b)
      (let () c)))

(define-syntax (or-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id))
     #'(begin)]))

(define-syntax (or-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id))
     #'(finish-id)]))
