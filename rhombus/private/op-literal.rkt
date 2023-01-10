#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (rename-in "ellipsis.rkt"
                    [... rhombus...])
         (rename-in "underscore.rkt"
                    [_ rhombus_])
         "dollar.rkt")

(provide (for-syntax
          :$
          :...
          :_))

(begin-for-syntax
  (define-syntax-class (:$ in-space)
    #:description "$"
    [pattern op:identifier
             #:when (free-identifier=? #'$ (in-space #'op))])

  (define-syntax-class (:... in-space)
    #:description "..."
    [pattern op:identifier
             #:when (free-identifier=? #'rhombus... (in-space #'op))])

  (define-syntax-class (:_ in-space)
    #:description "_"
    [pattern op:identifier
             #:when (free-identifier=? #'rhombus_ (in-space #'op))]))
