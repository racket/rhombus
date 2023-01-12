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

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/stxclass))

  (struct rhombus-syntax-class (kind class attributes splicing?))

  (struct syntax-class-attribute (id depth)))

(define-syntax (define-syntax-class-syntax stx)
  (syntax-parse stx
    [(_ name:id rhs)
     (quasisyntax/loc stx
       (define-syntax #,(in-syntax-class-space #'name) rhs))]))

(define-syntax Term (rhombus-syntax-class 'term #f null #f))
(define-syntax Id (rhombus-syntax-class 'term #'identifier null #f))
(define-syntax Op (rhombus-syntax-class 'term #':operator null #f))
(define-syntax Id_Op (rhombus-syntax-class 'term #':operator-or-identifier null #f))
(define-syntax-class-syntax Keyword (rhombus-syntax-class 'term #'keyword null #f))
(define-syntax-class-syntax String (rhombus-syntax-class 'term #'string null #f))
(define-syntax-class-syntax Integer (rhombus-syntax-class 'term #'exact-integer null #f))
(define-syntax Group (rhombus-syntax-class 'group #f null #f))
(define-syntax Multi (rhombus-syntax-class 'multi #f null #f))
(define-syntax Block (rhombus-syntax-class 'block #f null #f))
