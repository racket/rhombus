#lang racket/base
(require syntax/parse
         syntax/stx
         enforest/proc-name)

(provide pack-tail
         pack-tail*
         unpack-tail
         unpack-tail*)

(define (pack-tail tail)
  (if (stx-null? tail)
      #`(parens)
      #`(parens (group . #,tail))))

(define (pack-tail* stx depth)
  (cond
    [(eqv? depth 0) stx]
    [(eqv? depth 1) (pack-tail stx)]
    [else
     (pack-tail (for/list ([t (in-list (syntax->list stx))])
                  (pack-tail* t (sub1 depth))))]))

(define (unpack-tail packed-tail proc)
  (syntax-parse packed-tail
    [((~datum parens) ((~datum group) . tail)) #'tail]
    [((~datum parens)) #'()]
    [else
     (raise-result-error (if (symbol? proc) proc (proc-name proc))
                         "rhombus-syntax-list?"
                         packed-tail)]))

(define (unpack-tail* r depth)
  (cond
    [(eqv? depth 0) r]
    [(eqv? depth 1) (unpack-tail r 'unquote)]
    [else (for/list ([r (in-list (unpack-tail r 'unquote))])
            (unpack-tail* r (sub1 depth)))]))
