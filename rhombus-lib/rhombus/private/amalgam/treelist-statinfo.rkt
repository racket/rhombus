#lang racket/base
(require (for-syntax racket/base))

(provide (for-syntax
          (rename-out [get-treelist-static-infos indirect-get-treelist-static-infos])
          install-get-treelist-static-infos!))

(define-for-syntax get-treelist-static-infos #f)

(define-for-syntax (install-get-treelist-static-infos! get-static-infos)
  (set! get-treelist-static-infos get-static-infos))
