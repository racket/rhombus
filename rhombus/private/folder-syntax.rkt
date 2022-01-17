#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "folder.rkt"
         "name-root.rkt")

(provide folder)

(define-simple-name-root folder
  macro)

(define-syntax macro
  (lambda (stx)
    (raise-syntax-error #f "not supported, yet" stx)))
