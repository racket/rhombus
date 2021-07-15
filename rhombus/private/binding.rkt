#lang racket/base
(require syntax/parse)

(provide :binding-form)

;; To unpack a binding transformer result:
(define-syntax-class :binding-form
  (pattern ((~and variable-ids (_:identifier ...))
            matcher-form
            (~and syntax-ids (_:identifier ...))
            syntax-form)))
