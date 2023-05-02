#lang racket/base

(provide if/blocked)

(define-syntax-rule (if/blocked tst thn els)
  (if tst (let () thn) els))
