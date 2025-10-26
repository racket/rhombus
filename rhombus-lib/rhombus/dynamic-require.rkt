#lang racket/base
(require (lib "rhombus/private/dynamic_require.rhm"))

(provide rhombus-dynamic-require
         rhombus-dynamic-require-predicate)

(define (rhombus-dynamic-require mp name-or-names)
  (define who 'rhombus-dynamic-require)
  (unless (module-path? mp)
    (raise-argument-error who "module-path?" mp))
  (check-name who name-or-names)
  (dynamic_require mp name-or-names))

(define (rhombus-dynamic-require-predicate mp name-or-names)
  (define who 'rhombus-dynamic-require-preicate)
  (unless (module-path? mp)
    (raise-argument-error who "module-path?" mp))
  (check-name who name-or-names)
  (dynamic_require_predicate mp name-or-names))

(define (check-name who name-or-names)
  (unless (or (symbol? name-or-names)
              (and (pair? name-or-names)
                   (list? name-or-names)
                   (andmap symbol? name-or-names)))
    (raise-argument-error who "(or/s symbol? (listof symbol?))" name-or-names)))
