#lang racket/base

(provide #%call-result
         #%call-result-predicate)

(define #%call-result #f)

;; used by the entry-point protocol
(define #%call-result-predicate #f)
