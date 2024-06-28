#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt"
                     "class-method-result.rkt")
         (only-in racket/vector
                  vector-append)
         "treelist.rkt"
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         (submod "map.rkt" for-append)
         "append-key.rkt"
         "append-property.rkt"
         "call-result-key.rkt"
         "static-info.rkt"
         (only-in "string.rkt"
                  +&)
         (submod "set.rkt" for-append)
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "is-static.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     Appendable)
         (for-spaces (#f
                      rhombus/repet)
                     ++))

(define-values (prop:Appendable Appendable? Appendable-ref)
  (make-struct-type-property 'Appendable))

(define-annotation-syntax Appendable
  (identifier-annotation appendable? ((#%append general-append))))
(define (appendable? v)
  (or (treelist? v)
      (list? v)
      (immutable-hash? v)
      (immutable-set? v)
      (string? v)
      (bytes? v)
      (vector? v)
      (Appendable? v)))

(define-class-desc-syntax Appendable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&append)
                     #'#(#:abstract)
                     (hasheq 'append 0)
                     (hasheq 'append #'append-result)
                     '()
                     #f
                     #'()
                     '(append veneer)
                     ;; --------------------
                     #'Appendable
                     #'Appendable
                     #'prop:Appendable
                     #'prop:Appendable
                     #'Appendable-ref
                     #'Appendable-ref
                     #t
                     #f
                     null))))

(define-syntax append-result
  (method-result-maker
   (lambda ()
     (method-result #'(lambda (x) #t) #t 1 "Any" #'() 4))))

(define-for-syntax (parse-append form1 form2 self-stx form1-in
                                 static?
                                 appendable-static-info
                                 k)
  (define direct-append-id/maybe-boxed (appendable-static-info #'#%append))
  (define checked? (and direct-append-id/maybe-boxed
                        (box? (syntax-e direct-append-id/maybe-boxed))))
  (define direct-append-id (if checked?
                               (unbox (syntax-e direct-append-id/maybe-boxed))
                               direct-append-id/maybe-boxed))
  (define append-id (or direct-append-id
                        (if static?
                            (raise-syntax-error #f
                                                (string-append "specialization not known" statically-str)
                                                self-stx
                                                form1-in)
                            #'general-append)))
  (define si (or (syntax-local-static-info append-id #'#%call-result)
                 #'()))
  (k append-id
     (not checked?)
     form1 form2
     si))

(define-for-syntax (build-append append-id direct? form1 form2 orig-stxes)
  (relocate+reraw
   (respan (datum->syntax #f orig-stxes))
   (datum->syntax (quote-syntax here)
                  (if direct?
                      (list append-id form1 form2)
                      `(,#'let ([a1 ,form1]
                                [a2 ,form2])
                               (,#'check-appendable a1 a2)
                               (,append-id a1 a2))))))

(define-syntax ++
  (expression-infix-operator
   (lambda () `((,(expr-quote +&) . same)))
   'automatic
   (lambda (form1-in form2 self-stx)
     (define static? (is-static-context? self-stx))
     (define form1 (rhombus-local-expand form1-in))
     (parse-append
      form1 form2 self-stx form1-in
      static?
      (lambda (key) (syntax-local-static-info form1 key))
      (lambda (append-id direct? form1 form2 si)
        (wrap-static-info*
         (build-append append-id direct? form1 form2
                       (list form1-in self-stx form2))
         si))))
   'left))

(define-repetition-syntax ++
  (repetition-infix-operator
   (lambda () `((,(repet-quote +&) . same)))
   'automatic
   (lambda (form1 form2 self-stx)
     (define static? (is-static-context? self-stx))
     (syntax-parse form1
       [form1-info::repetition-info
        (build-compound-repetition
         self-stx
         (list form1 form2)
         (lambda (form1 form2)
           (parse-append
            form1 form2 self-stx form1
            static?
            (lambda (key)
              (repetition-static-info-lookup #'form1-info.element-static-infos key))
            (lambda (append-id direct? form1 form2 si)
              (values
               (build-append append-id direct? form1 form2
                             (list form1 self-stx form2))
               si)))))]))
   'left))

;; checking for the same `append` method relies on the fact that `class`
;; will generate a new procedure each time that `append` is overridden
(define (same-append? a b)
  (eq? a b))

(define append-who/op '++)
(define append-who/method 'Appendable.append)

(define (raise-mismatch what v1 v2
                        #:both-append? [both-append? #f])
  (define other-what (if both-append?
                         (string-append "other " what)
                         "other value"))
  (raise-arguments-error*
   append-who/op rhombus-realm
   (string-append "cannot append "
                  (case (string-ref what 0)
                    [(#\a #\i) "an "]
                    [else "a "])
                  what " and " other-what
                  (if both-append?
                      (string-append ";\n two "
                                     what
                                     "s must share the same `append` implementation")
                      ""))
   what v1
   other-what v2))

(define (general-append v1 v2)
  (cond
    [(treelist? v1)
     (unless (treelist? v2)
       (raise-mismatch "list" v1 v2))
     (treelist-append v1 v2)]
    [(list? v1)
     (unless (list? v2)
       (raise-mismatch "pair list" v1 v2))
     (append v1 v2)]
    [(immutable-hash? v1)
     (unless (immutable-hash? v2)
       (raise-mismatch "immutable map" v1 v2))
     (hash-append v1 v2)]
    [(immutable-set? v1)
     (unless (immutable-set? v2)
       (raise-mismatch "immutable set" v1 v2))
     (set-append v1 v2)]
    [(string? v1)
     (unless (string? v2)
       (raise-mismatch "string" v1 v2))
     (string-append-immutable v1 v2)]
    [(bytes? v1)
     (unless (bytes? v2)
       (raise-mismatch "byte string" v1 v2))
     (bytes-append v1 v2)]
    [(vector? v1)
     (unless (vector? v2)
       (raise-mismatch "array" v1 v2))
     (vector-append v1 v2)]
    [else
     (define app1 (appendable-ref v1 #f))
     (unless app1
       (raise-argument-error* append-who/method rhombus-realm "Appendable" v1))
     (define app2 (appendable-ref v2 #f))
     (unless (and app2 (same-append? app1 app2))
       (raise-mismatch "appendable object" v1 v2
                       #:both-append? (and app2 #t)))
     (app1 v1 v2)]))

(define (check-appendable a1 a2)
  (define app1 (appendable-ref a1 #f))
  (unless app1
    (raise-arguments-error*
     append-who/op rhombus-realm
     "checked `append` must be applied to an appendable object"
     "value" a1))
  (define app2 (appendable-ref a2 #f))
  (unless (and app2 (same-append? app1 app2))
    (raise-mismatch "appendable object" a1 a2
                    #:both-append? (and app2 #t))))
