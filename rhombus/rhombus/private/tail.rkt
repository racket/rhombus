#lang racket/base
(require syntax/parse
         syntax/stx
         enforest/proc-name)

(provide pack-tail
         unpack-tail)

(define parens-blank (syntax-property (datum->syntax #f 'parens) 'raw ""))
(define group-blank (syntax-property (datum->syntax #f 'group) 'raw ""))

;; assumes that `tail` is a syntax list, and wraps
;; it as a parenthesized group; an empty list turns into
;; parentheses with no groups
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

;; assumes that `packed-tail` represents a shrubbery,
;; and unpacks be removing outer parentheses
(define (unpack-tail packed-tail proc)
  (syntax-parse packed-tail
    [((~datum parens) ((~datum group) . tail)) #'tail]
    [((~datum parens)) #'()]
    [else
     (raise-result-error (if (symbol? proc) proc (proc-name proc))
                         "rhombus-syntax-list?"
                         packed-tail)]))
