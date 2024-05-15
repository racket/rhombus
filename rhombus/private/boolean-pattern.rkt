#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "binding.rkt"
         "static-info.rkt"
         "literal.rkt"
         "if-blocked.rkt")

(provide (for-space rhombus/bind
                    &&
                    \|\|
                    !))

(module+ for-class
  (provide (for-syntax make-and-binding)))

;; ----------------------------------------
;; &&

(define-binding-syntax &&
  (binding-infix-operator
   (lambda () `((,(bind-quote \|\|) . stronger)))
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
   null
   'automatic
   (lambda (lhs rhs stx)
     (syntax-parse (list lhs rhs)
       [(lhs-i::binding-form rhs-i::binding-form)
        (cond
          [(and (free-identifier=? #'lhs-i.infoer-id #'literal-infoer)
                (free-identifier=? #'rhs-i.infoer-id #'literal-infoer))
           (binding-form
            #'literal-infoer
            #`(#,@#'lhs-i.data #,@#'rhs-i.data))]
          [else
           (binding-form
            #'or-infoer
            #`(#,lhs #,rhs))])]))
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
                   #'(lhs rhs))]))

(define-syntax (or-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info)
        IF success fail)
     ;; preserve the textual order
     #`(IF ((lambda (right-k)
              (lhs.matcher-id arg-id lhs.data if/blocked
                              #t
                              (right-k)))
            (lambda ()
              (rhs.matcher-id arg-id rhs.data if/blocked
                              #t
                              #f)))
           success
           fail)]))

(define-syntax (or-committer stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs))
     #'(begin)]))

(define-syntax (or-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs))
     #'(begin)]))

;; ----------------------------------------
;; !

(define-binding-syntax !
  (binding-prefix-operator
   (lambda ()
     `((,(bind-quote &&) . stronger)
       (,(bind-quote \|\|) . stronger)))
   'automatic
   (lambda (form stx)
     (binding-form #'not-infoer form))))

(define-syntax (not-infoer stx)
  (syntax-parse stx
    [(_ static-infos form::binding-form)
     #:with impl::binding-impl #'(form.infoer-id () form.data)
     #:with info::binding-info #'impl.info
     (binding-info (string-append "!" (syntax-e #'info.annotation-str))
                   #'info.name-id
                   #'static-infos
                   #'()
                   #'not-matcher
                   #'not-committer
                   #'not-binder
                   #'info)]))

(define-syntax (not-matcher stx)
  (syntax-parse stx
    [(_ arg-id info::binding-info
        IF success fail)
     #'(IF (let ()
             (info.matcher-id arg-id info.data if/blocked
                              #f
                              #t))
           success
           fail)]))

(define-syntax (not-committer stx)
  (syntax-parse stx
    [(_ arg-id info)
     #'(begin)]))

(define-syntax (not-binder stx)
  (syntax-parse stx
    [(_ arg-id info)
     #'(begin)]))
