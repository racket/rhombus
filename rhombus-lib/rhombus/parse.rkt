#lang racket/base
(require (rename-in (submod "private/amalgam.rkt" parse)
                    [rhombus-expression parse:rhombus-expression]))

(provide rhombus-expression
         rhombus-top)

(define-syntax-rule (rhombus-expression e)
  ;; an expression may expand to `(begin (quote-syntax statinfo) expr)`,
  ;; so make sure that's not spliced in a definition context
  (let () (parse:rhombus-expression e)))
