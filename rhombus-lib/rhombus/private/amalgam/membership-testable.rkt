#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt"
                     "class-method-result.rkt")
         (only-in racket/vector
                  vector-member)
         "treelist.rkt"
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         (submod "range.rkt" for-container)
         "contains-key.rkt"
         "contains-property.rkt"
         "static-info.rkt"
         (submod "set.rkt" for-container)
         "repetition.rkt"
         "compound-repetition.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         "is-static.rkt"
         "order.rkt"
         "order-primitive.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     MembershipTestable)
         (for-spaces (#f
                      rhombus/repet)
                     in))

(module+ in-operator
  (provide (for-syntax :in)))

(define-values (prop:MembershipTestable MembershipTestable? MembershipTestable-ref)
  (make-struct-type-property 'MembershipTestable))

(define-annotation-syntax MembershipTestable
  (identifier-annotation container? ((#%contains general-contains))))
(define (container? v)
  (or (treelist? v)
      (list? v)
      (hash? v)
      (set? v)
      (vector? v)
      (range? v)
      (MembershipTestable? v)))

(define-class-desc-syntax MembershipTestable
  (interface-desc-maker
   (lambda ()
     (interface-desc #'()
                     '#(#&contains)
                     #'#(#:abstract)
                     (hasheq 'contains 0)
                     (hasheq 'contains #'contains-result)
                     '()
                     #f
                     #'()
                     '(contains veneer)
                     ;; --------------------
                     #'MembershipTestable
                     #'MembershipTestable
                     #'prop:MembershipTestable
                     #'prop:MembershipTestable
                     #'MembershipTestable-ref
                     #'MembershipTestable-ref
                     #t
                     #f
                     null))))

(define-syntax contains-result
  (method-result-maker
   (lambda ()
     (method-result #'(lambda (x) (boolean? x)) #t 1 "Boolean" #'() 4))))

(define-for-syntax (parse-contains form1 form2 self-stx form2-in
                                   static?
                                   container-static-info
                                   k)
  (define direct-contains-id (container-static-info #'#%contains))
  (define contains-id (or direct-contains-id
                             (if static?
                                 (raise-syntax-error #f
                                                     (string-append "specialization not known" statically-str)
                                                     self-stx
                                                     form2-in)
                                 #'general-contains)))
  (k contains-id form1 form2))

(define-for-syntax (build-contains contains-id form1 form2 mode orig-stxes)
  (relocate+reraw
   (respan (datum->syntax #f orig-stxes))
   (datum->syntax (quote-syntax here)
                  `(,#'let ([a1 ,form1]
                            [a2 ,form2])
                           ,(let ([r `(,contains-id a2 a1)])
                              (if (eq? mode 'invert)
                                  `(not ,r)
                                  r))))))

(begin-for-syntax
  (define-syntax-class :in
    #:description "a membership operator"
    #:opaque
    [pattern ::name
             #:when (free-identifier=? #'name
                                       (expr-quote in))]))

(define-syntax in
  (expression-infix-operator
   (lambda () (order-quote equivalence))
   '()
   'automatic
   (lambda (form1 form2-in self-stx [mode 'normal])
     (define static? (is-static-context? self-stx))
     (define form2 (rhombus-local-expand form2-in))
     (parse-contains
      form1 form2 self-stx form2-in
      static?
      (lambda (key) (syntax-local-static-info form2 key))
      (lambda (contains-id form1 form2)
        (build-contains contains-id form1 form2 mode
                           (list form1 self-stx form2-in)))))
   'left))

(define-repetition-syntax in
  (repetition-infix-operator
   (lambda () (order-quote equivalence))
   '()
   'automatic
   (lambda (form1 form2 self-stx [mode 'normal])
     (define static? (is-static-context? self-stx))
     (syntax-parse form1
       [form1-info::repetition-info
        (build-compound-repetition
         self-stx
         (list form1 form2)
         (lambda (form1 form2)
           (parse-contains
            form1 form2 self-stx form2
            static?
            (lambda (key)
              (repetition-static-info-lookup #'form1-info.element-static-infos key))
            (lambda (contains-id form1 form2)
              (values
               (build-contains contains-id form1 form2 mode
                                  (list form1 self-stx form2))
               #'())))))]))
   'left))

(define contains-who/method 'MembershipTestable.contains)

(define (general-contains v1 v2)
  (cond
    [(treelist? v1)
     (treelist-member? v1 v2)]
    [(list? v1)
     (and (member v1 v2) #t)]
    [(hash? v1)
     (hash-has-key? v1 v2)]
    [(set? v1)
     (hash-has-key? (set-ht v1) v2)]
    [(vector? v1)
     (and (vector-member v1 v2) #t)]
    [(range? v1)
     (Range.contains v1 v2)]
    [else
     (define in1 (contains-ref v1 #f))
     (unless in1
       (raise-annotation-failure contains-who/method v1 "MembershipTestable"))
     (in1 v1 v2)]))
