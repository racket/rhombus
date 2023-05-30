#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt"
                     "statically-str.rkt"
                     "interface-parse.rkt")
         "provide.rkt"
         "expression.rkt"
         "repetition.rkt"
         (submod "annotation.rkt" for-class)
         "parse.rkt"
         (submod "map.rkt" for-build)
         "append-key.rkt"
         "append-indirect-key.rkt"
         "append-property.rkt"
         "call-result-key.rkt"
         "function-indirect-key.rkt"
         "static-info.rkt"
         (only-in "string.rkt"
                  +&)
         (submod "set.rkt" for-ref)
         (submod "set.rkt" for-build)
         "repetition.rkt"
         "compound-repetition.rkt"
         "realm.rkt"
         (only-in "class-desc.rkt" define-class-desc-syntax)
         (only-in "class-method-result.rkt" method-result)
         "vector-append.rkt")

(provide (for-spaces (rhombus/class
                      rhombus/annot)
                     Appendable)
         ++)

(module+ for-dynamic-static
  (provide (for-spaces (#f
                        rhombus/repet)
                       ++
                       static-++)))

(define-values (prop:Appendable Appendable? Appendable-ref)
  (make-struct-type-property 'Appendable))

(define-annotation-syntax Appendable
  (identifier-annotation #'appendable? #'((#%append general-append))))
(define (appendable? v)
  (or (Appendable? v)
      (hash? v)
      (list? v)
      (vector? v)
      (set? v)
      (string? v)
      (bytes? v)))

(define-class-desc-syntax Appendable
  (interface-desc #'Appendable
                  #'Appendable
                  #'()
                  #'prop:Appendable
                  #'prop:Appendable
                  #'Appendable-ref
                  '#(#&append)
                  #'#(#:abstract)
                  (hasheq 'append 0)
                  #hasheq()
                  #t
                  '()
                  #f
                  #'()
                  '(append)))

(define-for-syntax (make-++-expression name static?)
  (expression-infix-operator
   name
   `((,(expr-quote +&) . same))
   'automatic
   (lambda (form1-in form2 stx)
     (define form1 (rhombus-local-expand form1-in))
     (define direct-append-id (syntax-local-static-info form1 #'#%append))
     (define static-append-id (or direct-append-id
                                  (syntax-local-static-info/indirect form1 #'#%append/checked #'#%append-indirect)))
     (define append-id (or static-append-id
                           (if static?
                               (raise-syntax-error '++ (string-append "specialization not known" statically-str) form1-in)
                               #'general-append)))
     (define si (or (syntax-local-static-info/indirect append-id #'#%call-result #'#%function-indirect) #'()))
     (wrap-static-info*
      (datum->syntax (quote-syntax here)
                     (if (or direct-append-id
                             (not static-append-id))
                         (list append-id form1 form2)
                         `(,#'let ([a1 ,form1]
                                   [a2 ,form2])
                                  (check-appendable a1 a2)
                                  (,append-id a1 a2)))
                     (span-srcloc form1 form2)
                     stx)
      si))
   'left))

(define-syntax ++ (make-++-expression (expr-quote ++) #f))
(define-syntax static-++ (make-++-expression (expr-quote static-++) #t))

(define-for-syntax (make-++-repetition name static?)
  (repetition-infix-operator
   name
   `((,(expr-quote +&) . same))
   'automatic
   (lambda (form1-in form2 stx)
     (raise-syntax-error #f "not yet ready" stx))
   'left))

(define-repetition-syntax ++ (make-++-repetition (expr-quote ++) #f))
(define-repetition-syntax static-++ (make-++-repetition (expr-quote static-++) #t))

;; checking for the same `append` method relies on the fact that `class`
;; will generate a new procedure each time that `append` is overridden
(define (same-append? a b)
  (eq? a b))

(define (general-append map1 map2)
  (define (mismatch what)
    (raise-arguments-error* '++ rhombus-realm
                            (format "cannot append a~a ~a and other value"
                                    (if (eqv? (string-ref what 0) #\a) "n" "")
                                    what)
                            what map1
                            "other value" map2))
  (cond
    [(list? map1) (cond
                    [(list? map2) (append map1 map2)]
                    [else (mismatch "list")])]
    [(hash? map1) (cond
                    [(hash? map2) (hash-append/proc map1 map2)]
                    [else (mismatch "map")])]
    [(set? map1) (cond
                   [(set? map2) (set-append/proc map1 map2)]
                   [else (mismatch "set")])]
    [(string? map1) (cond
                      [(string? map2) (string-append-immutable map1 map2)]
                      [else (mismatch "string")])]
    [(bytes? map1) (cond
                     [(bytes? map2) (bytes-append map1 map2)]
                     [else (mismatch "byte string" map1)])]
    [(appendable-ref map1 #f)
     => (lambda (app1)
          (cond
            [(appendable-ref map2 #f)
             => (lambda (app2)
                  (cond
                    [(same-append? app1 app2)
                     (app1 map1 map2)]
                    [else
                     (mismatch "appendable object")]))]
            [else (mismatch "appendable object")]))]
    [(vector? map1) (cond
                     [(vector? map2) (vector-append map1 map2)]
                     [else (mismatch "array")])]    
    [else (raise-argument-error* '++ rhombus-realm "Appendable" map1)]))

(define (check-appendable a1 a2)
  (cond
    [(appendable-ref a1 #f)
     => (lambda (app1)
          (unless (cond
                    [(appendable-ref a2 #f)
                     => (lambda (app2)
                          (same-append? app1 app2))]
                    [else #f])
            (raise-arguments-error* '++ rhombus-realm
                                    "cannot append an appendable object and other value"
                                    "appendable object" a1
                                    "other value" a2)))]
    [else
     ;; If we get here, then it means that static information was wrong
     (raise-argument-error* '++ rhombus-realm "Appendable" a1)]))
