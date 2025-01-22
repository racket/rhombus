#lang racket/base
(require "../version-case.rkt")

(provide vector-member)

;; TEMP approximate newer `vector-member`
(meta-if-version-at-least
 "8.15.0.1"
 (require (only-in racket/vector
                   vector-member))
 (define (vector-member val vec [eql equal?])
   (for/or ([elem (in-vector vec)]
            [idx (in-naturals 0)])
     (and (eql val elem)
          idx))))
