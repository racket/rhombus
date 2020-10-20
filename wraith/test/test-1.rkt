#lang wraith racket

module+ test
  require rackunit

define (interp env s)
  match s
    x #:when (symbol? x)
      hash-ref env x
    `(λ (,x) ,b)
     λ (xv)
       interp (hash-set env x xv) b
    (list f a)
      (interp env f) (interp env a)

define three interp (hash)
               'λ (f)
                 λ (x)
                  f (f (f x))
  

module+ test
  check-equal? ((three add1) 0) 3

