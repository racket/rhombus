#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "pack.rkt"
         "pattern-variable.rkt"
         "unquote-binding.rkt")

(provide (for-syntax identifier-as-unquote-binding))

(define-for-syntax (identifier-as-unquote-binding id kind
                                                  #:result [result list]
                                                  #:pattern-variable [pattern-variable list])
  (define-values (pack* unpack*)
    (case kind
      [(term) (values #'pack-term* #'unpack-term*)]
      [(group) (values #'pack-group-or-empty* #'unpack-group-or-empty*)]
      [(multi block) (values #'pack-tagged-multi* #'unpack-multi-as-term*)]))
  (let* ([temps (generate-temporaries (list id id))]
         [temp1 (car temps)]
         [temp2 (cadr temps)])
    (result temp1
            (list #`[#,temp2 (#,pack* (syntax #,temp1) 0)])
            (list (make-pattern-variable-bind id temp2 unpack* 0 null))
            (list (pattern-variable (syntax-e id) id temp2 0 unpack*)))))
