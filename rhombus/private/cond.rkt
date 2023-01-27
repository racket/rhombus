#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "expression.rkt"
         "parse.rkt"
         "else-clause.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_])
         "error.rkt")

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]
                     [rhombus-when when]
                     [rhombus-unless unless]))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts)
       [(form-id test ... (alts alt ...)
                 . tail)
        (syntax-parse #'(alt ...)
          #:datum-literals (block)
          [(((~and tag-thn block) thn ...)
            ((~and tag-els block) els ...))
           (values
            #'(if (rhombus-expression (group test ...))
                  (rhombus-body-at tag-thn thn ...)
                  (rhombus-body-at tag-els els ...))
            #'tail)]
          [_
           (raise-syntax-error #f
                               "expected two alternatives"
                               stx)])]))))

(define-syntax rhombus-cond
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id (alts
                  (block (group pred ... ((~and tag block) rhs ...)))
                  ...
                  e::else-clause)
                 . tail)
        (values
         #'(cond
             [(rhombus-expression (group pred ...))
              (rhombus-body-at tag rhs ...)]
             ...
             [else e.parsed])
         #'tail)]
       [(form-id (alts
                  (block (group pred ... ((~and tag block) rhs ...)))
                  ...)
                 . tail)
        (values
         #'(cond
             [(rhombus-expression (group pred ...))
              (rhombus-body-at tag rhs ...)]
             ...
             [else (cond-fallthrough 'form-id)])
         #'tail)]
       [(form-id (block) . tail)
        (values
         #'(cond-fallthrough 'form-id)
         #'tail)]))))

(define (cond-fallthrough who)
  (raise-contract-error who "no matching case"))

(define-syntax rhombus-when
  (expression-transformer
   (lambda (stx)
     (parse-when stx #'when))))

(define-syntax rhombus-unless
  (expression-transformer
   (lambda (stx)
     (parse-when stx #'unless))))

(define-for-syntax (parse-when stx racket-form-id)
  (syntax-parse stx
    #:datum-literals (alts)
    [(form-id test ... (alts alt ...)
              . tail)
     (syntax-parse #'(alt ...)
       #:datum-literals (block)
       [(((~and tag-thn block) thn ...))
        (values
         #`(#,racket-form-id (rhombus-expression (group test ...))
            (rhombus-body-at tag-thn thn ...))
         #'tail)]
       [_
        (raise-syntax-error #f
                            "expected a single alternative"
                            stx)])]))
