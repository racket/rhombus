#lang racket/base

(provide convert-static-infos)

(define (convert-static-infos static-infos-flatten
                              get-static-infos
                              convert-static-info)
  (for/list ([static-info (in-list (static-infos-flatten (get-static-infos)))])
    (convert-static-info static-info)))
