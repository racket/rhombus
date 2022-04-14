#lang racket

(provide all-values)

(require "types.rkt")

(define strings
  (list "" "abc" "apple" "Apple" "hello" "blahlbahblahblah"))

(define numbers
  (list 5 100032 23 4423 0 -23))

(define lists
  (list
   (list 1 2 3)
   (list 2 1 3)
   (list 2 1 3 5 31)
   (list 'a 1 "abc" 5 31)
   (list 'a (list 4 6) "abc" 5 31)
   (list 'a (list 6 4) "abc" 5 31)))

(define persons
  (list (person "Abe" 30)
        (person "Abby" 12)
        (person "Ferdy" 14)
        (person "Imp" 19)
        (person "" 5)
        (person "Blah" 0)))

(define all-values
  (append strings
          numbers
          lists
          persons
          ))
