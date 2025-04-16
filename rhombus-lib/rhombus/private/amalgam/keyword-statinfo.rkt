#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax
          (rename-out [get-keyword-static-infos indirect-get-keyword-static-infos])
          install-get-keyword-static-infos!))

(define-for-syntax get-keyword-static-infos #f)

(define-for-syntax (install-get-keyword-static-infos! get-static-infos)
  (set! get-keyword-static-infos get-static-infos))
