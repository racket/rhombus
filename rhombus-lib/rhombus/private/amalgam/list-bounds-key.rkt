#lang racket/base
(require (for-syntax racket/base)
         "static-info.rkt")

(define-for-syntax (bounds-union b1 b2)
  (with-syntax ([(_ min1 max1) b1]
                [(_ min2 max2) b2])
    #`(group
       #,(max (syntax-e #'min1) (syntax-e #'min2))
       #,(if (syntax-e #'max1)
             (if (syntax-e #'max2)
                 (min (syntax-e #'max1) (syntax-e #'max2))
                 #'max1)
             #'max2))))

(define-for-syntax (bounds-intersect b1 b2)
  (with-syntax ([(_ min1 max1) b1]
                [(_ min2 max2) b2])
    #`(group
       #,(min (syntax-e #'min1) (syntax-e #'min2))
       #,(and (syntax-e #'max1)
              (syntax-e #'max2)
              (max (syntax-e #'max1) (syntax-e #'max2))))))

(define-static-info-key-syntax/provide #%list-bounds
  (static-info-key bounds-union
                   bounds-intersect))

(define-static-info-key-syntax/provide #%treelist-bounds
  (static-info-key bounds-union
                   bounds-intersect))
