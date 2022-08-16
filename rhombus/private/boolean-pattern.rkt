#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "annotation-string.rkt")
         "binding.rkt"
         "parse.rkt")

(provide (for-space rhombus/binding
                    &&
                    \|\|))

;; ----------------------------------------
;; &&

(define-binding-syntax &&
  (binding-infix-operator
   #'&&
   (list (cons #'\|\| 'stronger))
   'macro
   (lambda (lhs stx)
     (syntax-parse stx
       [rhs::infix-op+binding+tail
        #:with lhs-parsed lhs
        (values (binding-form
                 #'and-infoer
                 #'(lhs-parsed rhs.parsed))
                #'rhs.tail)]))
   'left))

(define-syntax (and-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with (lhs-static-info ...) #'lhs.static-infos
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id (lhs-static-info ... . static-infos) rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with (lhs-bind-info ...) #'lhs.bind-infos
     (binding-info (annotation-string-and (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   #'(lhs-static-info ... . rhs.static-infos)
                   #'(lhs-bind-info ... . rhs.bind-infos)
                   #'and-matcher
                   #'and-binder
                   #'(lhs rhs))]))

(define-syntax (and-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info) IF success fail)
     #'(lhs.matcher-id arg-id lhs.data IF
                       (rhs.matcher-id arg-id rhs.data IF success fail)
                       fail)]))

(define-syntax (and-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info))
     #'(begin
         (lhs.binder-id arg-id lhs.data)
         (rhs.binder-id arg-id rhs.data))]))

;; ----------------------------------------
;; ||

(define-binding-syntax \|\|
  (binding-infix-operator
   #'\|\|
   null
   'macro
   (lambda (lhs stx)
     (syntax-parse stx
       [rhs::infix-op+binding+tail
        #:with lhs-parsed lhs
        (values (binding-form
                 #'or-infoer
                 #'(lhs-parsed rhs.parsed))
                #'rhs.tail)]))
   'left))

(define-syntax (or-infoer stx)
  (syntax-parse stx
    [(_ static-infos (lhs-i::binding-form rhs-i::binding-form))
     #:with lhs-impl::binding-impl #'(lhs-i.infoer-id static-infos lhs-i.data)
     #:with lhs::binding-info #'lhs-impl.info
     #:with (lhs-static-info ...) #'lhs.static-infos
     #:with rhs-impl::binding-impl #'(rhs-i.infoer-id static-infos rhs-i.data)
     #:with rhs::binding-info #'rhs-impl.info
     #:with (lhs-bind-info ...) #'lhs.bind-infos
     (binding-info (annotation-string-or (syntax-e #'lhs.annotation-str) (syntax-e #'rhs.annotation-str))
                   #'lhs.name-id
                   #'()
                   #'()
                   #'or-matcher
                   #'or-binder
                   #'(lhs rhs finish))]))

(define-syntax (or-matcher stx)
  (syntax-parse stx
    [(_ arg-id (lhs::binding-info rhs::binding-info finish-id) IF success fail)
     #'(begin
         (define finish-id
           (let ()
             (lhs.matcher-id arg-id lhs.data block-if
                             (lambda ()
                               (lhs.binder-id arg-id lhs.data)
                               (void))
                             (rhs.matcher-id arg-id rhs.data block-if
                                             (lambda ()
                                               (rhs.binder-id arg-id rhs.data)
                                               (void))
                                             #f))))
         (IF finish-id success fail))]))

(define-syntax-rule (block-if a b c)
  (if (let () a)
      (let () b)
      (let () c)))

(define-syntax (or-binder stx)
  (syntax-parse stx
    [(_ arg-id (lhs rhs finish-id))
     #'(finish-id)]))
