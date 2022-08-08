#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "introducer.rkt")
         syntax/parse
         "operator-parse.rkt"
         "name-root.rkt"
         "definition.rkt")

(provide Term
         Id
         Op
         Id_Op
         Keyw
         Group
         Block
         Multi)

(module+ for-quasiquote
  (begin-for-syntax
    (provide in-syntax-class-space
             (struct-out rhombus-syntax-class)
             (struct-out syntax-class-attribute))))
                       
(module+ for-syntax-class-syntax
  (provide (for-syntax rhombus-syntax-class in-syntax-class-space)))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/syntax-class))

  (struct rhombus-syntax-class (kind class attributes splicing?))

  (struct syntax-class-attribute (id depth)))

(define-syntax Term (rhombus-syntax-class 'term #f null #f))
(define-syntax Id (rhombus-syntax-class 'term #'identifier null #f))
(define-syntax Op (rhombus-syntax-class 'term #':operator null #f))
(define-syntax Id_Op (rhombus-syntax-class 'term #':operator-or-identifier null #f))
(define-syntax Keyw (rhombus-syntax-class 'term #'keyword null #f))
(define-syntax Group (rhombus-syntax-class 'group #f null #f))
(define-syntax Multi (rhombus-syntax-class 'multi #f null #f))
(define-syntax Block (rhombus-syntax-class 'block #f null #f))
