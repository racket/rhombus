#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parens.rkt"
         "parse.rkt"
         (submod "ellipsis.rkt" for-parse)
         "op-literal.rkt"
         "repetition.rkt"
         "static-info.rkt")

(provide all
         any)

(define-for-syntax (combiner comb for/comb 0-value
                             merge-static-infoss)
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ (_::parens) . tail)
        (values 0-value #'tail)]
       [(_ (_::parens g ...) . tail)
        (define-values (e rev-statinfoss)
          (let loop ([gs #'(g ...)]
                     [rev-statinfoss '()])
            (define (combine left-e statinfos gs)
              (cond
                [(null? (syntax-e gs))
                 (values left-e (cons statinfos rev-statinfoss))]
                [else
                 (define-values (right-e new-rev-statinfoss)
                   (loop gs (cons statinfos rev-statinfoss)))
                 (values #`(#,comb #,left-e #,right-e) new-rev-statinfoss)]))
            (syntax-parse gs
              #:datum-literals (group)
              [(g (group _::...-expr) . pre-tail)
               (define-values (tail count) (consume-extra-ellipses #'pre-tail))
               (syntax-parse #'g
                 [repet::repetition
                  #:with repet-info::repetition-info #'repet.parsed
                  (combine (render-repetition for/comb #'repet.parsed #:depth (add1 count))
                           #'repet-info.element-static-infos
                           tail)])]
              [(e::expression . tail)
               (define parsed-expr (rhombus-local-expand #'e.parsed))
               (combine (discard-static-infos parsed-expr)
                        (extract-static-infos parsed-expr)
                        #'tail)])))
        (define statinfos (merge-static-infoss rev-statinfoss))
        (values (wrap-static-info* e statinfos) #'tail)]))))

(define-syntax all
  (combiner #'and #'for/and #''#t
            (lambda (rev-statinfoss)
              (define last-statinfos (car rev-statinfoss))
              (if (not (static-infos-empty? last-statinfos))
                  #`((#%maybe #,last-statinfos))
                  #'()))))

(define-syntax any
  (combiner #'or #'for/or #''#f
            (lambda (rev-statinfoss)
              (define (demaybe si)
                (define maybe-si (static-info-lookup si #'#%maybe))
                (if maybe-si
                    (static-infos-and si maybe-si)
                    si))
              (define last-statinfos (car rev-statinfoss))
              (define butlast-statinfoss (reverse (cdr rev-statinfoss)))
              (cond
                [(null? butlast-statinfoss)
                 last-statinfos]
                [else
                 (define fst-statinfos (demaybe (car butlast-statinfoss)))
                 (define rst-statinfos (append (map demaybe (cdr butlast-statinfoss))
                                               (list last-statinfos)))
                 (for/fold ([left-statinfos fst-statinfos])
                           ([right-statinfos (in-list rst-statinfos)])
                   (static-infos-or left-statinfos right-statinfos))]))))
