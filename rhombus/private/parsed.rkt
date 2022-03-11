#lang racket/base
(require (for-syntax racket/base
                     "pack.rkt")
         "parse.rkt")

(provide (for-syntax parsed
                     unparsed))

(begin-for-syntax
  (define (unpack-parsed v who)
    (cond
      [(list? v) (map (lambda (v)
                        (unpack-parsed v who))
                      v)]
      [(syntax? v) (unpack-term v who)]
      [else v]))
  
  (define (parsed v)
    #`(parsed #,(unpack-parsed v 'parsed)))

  (define (unparsed d)
    #`(rhombus-expression #,(unpack-group d 'unparsed))))
