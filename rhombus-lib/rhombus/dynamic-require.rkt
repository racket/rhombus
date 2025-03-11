#lang racket/base
(require (lib "rhombus/private/dynamic_require.rhm"))

(provide rhombus-dynamic-require)

(define (rhombus-dynamic-require mp name-or-names)
  (define who 'rhombus-dynamic-require)
  (unless (module-path? mp)
    (raise-argument-error who "module-path?" mp))
  (unless (or (symbol? name-or-names)
              (and (pair? name-or-names)
                   (list? name-or-names)
                   (andmap symbol? name-or-names)))
    (raise-argument-error who "(or/s symbol? (listof symbol?))" mp))
  (dynamic_require mp name-or-names))
