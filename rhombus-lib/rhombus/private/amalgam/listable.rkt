#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "interface-parse.rkt"
                     "class-method-result.rkt"
                     "annot-context.rkt")
         "provide.rkt"
         (submod "list.rkt" for-listable)
         (submod "list.rkt" for-compound-repetition)
         (submod "annotation.rkt" for-class)
         "name-root.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "define-arity.rkt"
         (submod "dot.rkt" for-dot-provider)
         "call-result-key.rkt"
         "dot-provider-key.rkt"
         "dot-parse.rkt"
         "binding.rkt"
         "index-result-key.rkt"
         "static-info.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/namespace
                      rhombus/annot)
                     Listable))

(module+ for-static-info
  (provide (for-syntax get-listable-static-infos)))

(define-class-desc-syntax Listable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&to_list)
                     #'#(#:abstract)
                     (hasheq 'to_list 0)
                     (hasheq 'to_list #'to-list-result)
                     '()
                     #f
                     #'()
                     '()
                     ;; --------------------
                     #'Listable
                     #'Listable
                     #'prop:Listable
                     #'prop:Listable
                     #'Listable-ref
                     #'Listable-ref
                     #t
                     #f
                     null))))

(define-syntax to-list-result
  (method-result-maker
   (lambda ()
     (method-result #'treelist? #t 1 "List" (get-treelist-static-infos) 2))))

(define-for-syntax (get-listable-static-infos)
  #`((#%dot-provider listable-instance)))

(define-for-syntax (listable-expect-of-predicate predicate-stxs)
  #`(lambda (arg) #t))

(define-syntax (listable-of-static-infoss data static-infoss)
  #`((#%index-result #,(car static-infoss))))

(define-annotation-constructor (Listable Listable.assume_of)
  ()
  #'listable? #,(get-listable-static-infos)
  1
  #f
  listable-expect-of-predicate
  #'listable-of-static-infoss #f
  "converter annotation not supported for elements"
  #'())

(define-dot-provider-syntax listable-instance
  (dot-provider
   (dot-parse-dispatch
    (lambda (field-sym field-proc ary nary repetition? fail-k)
      (case field-sym
        [(to_list) (nary 1 #'Listable.to_list #'Listable.to_list/method)]
        [else (fail-k)])))))

(define-name-root Listable
  #:fields
  ([to_list Listable.to_list]
   [assume_of Listable.assume_of]
   [expect_of Listable.assume_of #:deprecate (rhombus/annot) "15-Mar-2026"]))

(define-syntax to-list-static-infos
  (lambda (data deps)
    (define si (get-treelist-static-infos))
    (define args (annotation-dependencies-args deps))
    (cond
      [(pair? args)
       (define elem-si (static-info-lookup (car args) #'#%index-result))
       (if elem-si
           #`((#%index-result #,elem-si)
              #,@si)
           si)]
      [else si])))

(define/method (Listable.to_list v)
  #:static-infos ((#%call-result ((#%dependent-result (to-list-static-infos #f)))))
  (to-treelist who v))

(define-annotation-syntax Listable.to_list
  (identifier-binding-annotation #,(binding-form #'to_list-infoer
                                                 #'val)
                                 val
                                 #,(get-treelist-static-infos)))

(define-syntax (to_list-infoer stx)
  (syntax-parse stx
    [(_ up-static-infos val)
     (binding-info "Listable.to_list"
                   #'val
                   (static-infos-and (get-listable-static-infos) #'up-static-infos)
                   #'((val ([#:repet ()])))
                   #'empty-oncer
                   #'to_list-matcher
                   #'()
                   #'to_list-committer
                   #'to_list-binder
                   #'(converted-val val))]))

(define-syntax (to_list-matcher stx)
  (syntax-parse stx
    [(_ arg-id (converted-val val) IF success fail)
     #'(IF (listable? arg-id) success fail)]))

(define-syntax (to_list-committer stx)
  (syntax-parse stx
    [(_ arg-id () (converted-val val))
     #'(define converted-val (to-treelist #f arg-id))]))

(define-syntax (to_list-binder stx)
  (syntax-parse stx
    [(_ arg-id () (converted-val val))
     #'(define val converted-val)]))
