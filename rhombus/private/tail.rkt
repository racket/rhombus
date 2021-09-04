#lang racket/base
(require syntax/parse
         syntax/stx
         enforest/proc-name)

(provide pack-tail
         pack-tail*
         unpack-tail
         unpack-tail*)

(define parens-blank (syntax-property (datum->syntax #f 'parens) 'raw ""))
(define group-blank (syntax-property (datum->syntax #f 'group) 'raw ""))

(define (pack-tail tail #:after [after #f])
  (if (stx-null? tail)
      (cond
        [(and after
              (syntax-position after)
              (syntax-span after))
         (define loc (srcloc (syntax-source after)
                             (syntax-line after)
                             (let ([col (syntax-column after)])
                               (and col (+ col (syntax-span after))))
                             (+ (syntax-position after) (syntax-span after))
                             0))
         #`(#,(syntax-property (syntax/loc loc parens) 'raw ""))]
        [else #`(#,parens-blank)])
      #`(#,parens-blank (#,group-blank . #,tail))))

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
