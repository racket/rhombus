#lang racket

(provide type)

(require qi
         "types.rkt")

(define-switch type
  [string? 'string]
  [number? 'number]
  [list? 'list]
  [person? 'person]
  [else #f])
