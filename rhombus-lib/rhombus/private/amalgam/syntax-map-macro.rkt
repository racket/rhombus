#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "definition.rkt"
         (submod "key-comp-macro.rkt" for-key-comp)
         "syntax-map.rkt")

(provide (for-space rhombus/defn
                    define_equal_name_and_scopes))

(define-defn-syntax define_equal_name_and_scopes
  (definition-transformer
    (lambda (stx name-prefix)
      (syntax-parse stx
        #:datum-literals (group)
        [(_ id)
         (build-key-comp #'id
                         #f
                         #'equal-name-and-scopes-map?
                         #'wrap-equal-name-and-scopes-map)]))))
