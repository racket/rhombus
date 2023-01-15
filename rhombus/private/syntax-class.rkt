#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "introducer.rkt")
         syntax/parse/pre
         "operator-parse.rkt"
         "name-root.rkt"
         "definition.rkt")

(provide Term
         Id
         Op
         Id_Op
         (for-space rhombus/stxclass
                    Keyword
                    String
                    Integer)
         Group
         Block
         Multi)

(module+ for-quasiquote
  (begin-for-syntax
    (provide in-syntax-class-space
             (struct-out rhombus-syntax-class)
             (struct-out syntax-class-attribute))))
                       
(module+ for-syntax-class-syntax
  (provide (for-syntax rhombus-syntax-class in-syntax-class-space)
           define-syntax-class-syntax))

(module+ for-syntax-class
  (provide (for-syntax make-syntax-class)
           define-operator-syntax-classes))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/stxclass))

  (struct rhombus-syntax-class (kind class attributes splicing? arity))

  (struct syntax-class-attribute (id depth))

  (define (make-syntax-class pat
                             #:kind [kind 'term]
                             #:arity [arity #f]
                             #:fields [fields null]
                             #:splicing? [splicing? #f])
    (rhombus-syntax-class kind pat fields splicing? arity)))

(define-syntax (define-syntax-class-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-class-space #'name) rhs))]))

(define-syntax Term (make-syntax-class #f))
(define-syntax Id (make-syntax-class #'identifier))
(define-syntax Op (make-syntax-class #':operator))
(define-syntax Id_Op (make-syntax-class #':operator-or-identifier))
(define-syntax-class-syntax Keyword (make-syntax-class #'keyword))
(define-syntax-class-syntax String (make-syntax-class #'string))
(define-syntax-class-syntax Integer (make-syntax-class #'exact-integer))
(define-syntax Group (make-syntax-class #f #:kind 'group))
(define-syntax Multi (make-syntax-class #f #:kind 'multi))
(define-syntax Block (make-syntax-class #f #:kind 'block))

(define-syntax-rule (define-operator-syntax-classes
                      Group :form
                      AfterPrefixGroup :prefix-op+form+tail
                      AfterInfixGroup :infix-op+form+tail)
  (begin
    (define-syntax Group (make-syntax-class #':form
                                            #:kind 'group
                                            #:fields '((parsed . 0))))
    (define-syntax AfterPrefixGroup (make-syntax-class #':prefix-op+form+tail
                                                       #:kind 'group
                                                       #:arity 2
                                                       #:fields '((parsed . 0) (tail . 0))))
    (define-syntax AfterInfixGroup (make-syntax-class #':infix-op+form+tail
                                                      #:kind 'group
                                                      #:arity 2
                                                      #:fields '((parsed . 0) (tail . 0))))))

