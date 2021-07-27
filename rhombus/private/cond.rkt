#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "parse.rkt")

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]
                     [rhombus-else else]))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block)
       [(form-id test ... (alts
                           (block thn ...)
                           (block els ...))
                 . tail)
        (values
         #'(if (rhombus-expression (group test ...))
               (rhombus-block thn ...)
               (rhombus-block els ...))
         #'tail)]))))

(define-syntax rhombus-cond
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       #:literals (rhombus-else)
       [(form-id (alts
                  (block (group pred ... (block rhs ...)))
                  ...
                  (block (group rhombus-else (block else-rhs ...))))
                 . tail)
        (values
         #'(cond
             [(rhombus-expression (group pred ...))
              (rhombus-block rhs ...)]
             ...
             [else
              (rhombus-block else-rhs ...)])
         #'tail)]
       [(form-id (alts
                  (block (group pred ... (block rhs ...)))
                  ...)
                 . tail)
        (values
         #'(cond
             [(rhombus-expression (group pred ...))
              (rhombus-block rhs ...)]
             ...)
         #'tail)]))))

(define-syntax rhombus-else #f)
