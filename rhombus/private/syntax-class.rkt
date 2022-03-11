#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "introducer.rkt")
         syntax/parse
         "operator-parse.rkt"
         "name-root.rkt"
         "definition.rkt")

(provide $:
         Term
         Id
         Op
         Id_Op
         Keyw
         Group
         Block
         Multi)

(module+ for-macro
  (provide syntax))

(module+ for-quasiquote
  (provide (for-syntax in-syntax-class-space
                       rhombus-syntax-class?
                       rhombus-syntax-class-kind
                       rhombus-syntax-class-class)))

(begin-for-syntax
  (define in-syntax-class-space (make-interned-syntax-introducer/add 'rhombus/syntax-class))

  (struct rhombus-syntax-class (kind class)))

(define-syntax $: "only in patterns")
(define-syntax Term (rhombus-syntax-class 'term #f))
(define-syntax Id (rhombus-syntax-class 'term #'identifier))
(define-syntax Op (rhombus-syntax-class 'term #':operator))
(define-syntax Id_Op (rhombus-syntax-class 'term #':operator-or-identifier))
(define-syntax Keyw (rhombus-syntax-class 'term #'keyword))
(define-syntax Group (rhombus-syntax-class 'group #f))
(define-syntax Multi (rhombus-syntax-class 'multi #f))
(define-syntax Block (rhombus-syntax-class 'block #f))

(define-simple-name-root syntax
  class)

(define-syntax class
  (definition-transformer
    (lambda (stx)
      (raise-syntax-error 'syntax.class "not supported, yet" stx))))
