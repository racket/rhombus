#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax
          (rename-out [get-map-static-infos indirect-get-map-static-infos])
          install-get-map-static-infos!))

(define-for-syntax get-map-static-infos #f)

(define-for-syntax (install-get-map-static-infos! get-static-infos)
  (set! get-map-static-infos get-static-infos))
