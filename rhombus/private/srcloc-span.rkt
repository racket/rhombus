#lang racket/base
(require shrubbery/property
         syntax/parse/pre
         racket/syntax-srcloc
         "srcloc.rkt")

(provide relocate-span
         relocate*
         relevant-source-syntax)

;; Similar to "respan" from "srcloc.rkt", but also propagates
;; raw-prefix and raw-suffix properties
(define (relocate-span stx ctx-stxes-in)
  (define ctx-stx (if (null? (cdr ctx-stxes-in))
                      (maybe-respan (car ctx-stxes-in))
                      (respan (datum->syntax #f (map maybe-respan ctx-stxes-in)))))
  (define new-stx (relocate ctx-stx stx))
  (define (combine-raw a b) (if (null? a) b (if (null? b) a (cons a b))))
  (define head (relevant-source-syntax (car ctx-stxes-in)))
  (define tail (relevant-source-syntax (let loop ([ctx-stxes-in ctx-stxes-in])
                                         (if (null? (cdr ctx-stxes-in))
                                             (car ctx-stxes-in)
                                             (loop (cdr ctx-stxes-in))))))
  (let* ([new-stx (syntax-raw-prefix-property new-stx (syntax-raw-prefix-property head))]
         [new-stx (syntax-raw-suffix-property new-stx
                                              (combine-raw
                                               (or (syntax-raw-tail-suffix-property tail) null)
                                               (or (syntax-raw-suffix-property tail) null)))]
         [new-stx (syntax-opaque-raw-property new-stx (extract-raw ctx-stx #f #f))])
    new-stx))

(define (relocate* stx ctx-stx-in)
  (define ctx-stx (relevant-source-syntax ctx-stx-in))
  #;(log-error "?? ~s" (syntax->datum stx))
  #;(log-error " : ~s" (syntax->datum ctx-stx-in))
  #;(log-error " = ~s" (syntax->datum ctx-stx))
  (define (relocate stx)
    #;(log-error " ! ~s" (syntax->datum stx))
    (datum->syntax stx (syntax-e stx) ctx-stx ctx-stx))
  (let loop ([stx stx])
    (syntax-parse stx
      #:datum-literals (group block alts parens brackets braces quotes multi op)
      [((~and head (~or group block alts parens brackets braces quotes)) . rest)
       (datum->syntax #f (cons (relocate #'head) #'rest))]
      [((~and m multi) (g t))
       #:when (syntax-property #'g 'from-pack)
       (loop #'t)]
      [((~and m multi) (g . rest))
       (datum->syntax #f (list #'m (cons (relocate #'g) #'rest)))]
      [((~and tag op) o)
       (datum->syntax #f (list #'tag (relocate #'o)))]
      [_
       (relocate stx)])))

(define (relevant-source-syntax ctx-stx-in)
  (syntax-parse ctx-stx-in
    #:datum-literals (group block alts parens brackets braces quotes multi op)
    [((~and head (~or group block alts parens brackets braces quotes)) . _) #'head]
    [(multi (g t))
     #:when (syntax-property #'g 'from-pack)
     (relevant-source-syntax #'t)]
    [(multi (g . _)) #'g]
    [(op o) #'o]
    [_ ctx-stx-in]))
