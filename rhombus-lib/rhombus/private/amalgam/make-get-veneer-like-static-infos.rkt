#lang racket/base

(provide make-get-veneer-like-static-infos)

(define (make-get-veneer-like-static-infos static-infos-flatten
                                           get-static-infos
                                           convert-static-info)
  (for/list ([static-info (in-list (static-infos-flatten (get-static-infos)))])
    (convert-static-info static-info)))
