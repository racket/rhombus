#lang racket/base
(require syntax/parse
         "tail.rkt")

(provide unpack-static-infos
         pack-static-infos)

(define (unpack-static-infos v)
  (pack-tail
   (syntax-parse v
     [((key val) ...)
      #'((parens (group key) (group val)) ...)])))

(define (pack-static-infos v who)
  (datum->syntax
   #f
   (map (lambda (p)
          (syntax-parse p
            #:datum-literals (group parens)
            [(parens (group key) (group val))
             #'(key val)]))
        (syntax->list (unpack-tail v who)))))
