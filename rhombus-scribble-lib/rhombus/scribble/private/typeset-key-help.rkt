#lang racket/base
(require shrubbery/print)

(provide target-id-key-symbol)

(define (target-id-key-symbol target-id)
  ;; Use the printed form of the name, instead of `(syntax-e target-id)`,
  ;; because we want a stable name that does not depend on the identifier
  ;; chosen by macro expansion to serve as the binding.
  (string->symbol (shrubbery-syntax->string target-id)))
