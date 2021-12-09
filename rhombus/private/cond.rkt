#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "parse.rkt"
         (only-in "underscore.rkt"
                  [_ rhombus-_]))

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]))

(define-syntax rhombus-if
  (expression-transformer
   #'rhombus-if
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
   #'rhombus-cond
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id (alts
                  (block (group pred ... ((~and tag block) rhs ...)))
                  ...
                  (block (group (~or #:else (~literal rhombus-_))
                                ((~and else-tag block) else-rhs ...))))
                 . tail)
        (values
         #'(cond
             [(rhombus-expression (group pred ...))
              (rhombus-body-at tag rhs ...)]
             ...
             [else
              (rhombus-body-at else-tag else-rhs ...)])
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
             [else (cond-fallthrough #'form-id)])
         #'tail)]
       [(form-id (block) . tail)
        (values
         #'(cond-fallthrough 'form-id)
         #'tail)]))))

(define (cond-fallthrough who)
  (error who "no matching case"))
