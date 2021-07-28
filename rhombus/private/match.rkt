#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "function.rkt"
         (only-in "cond.rkt"
                  [else rhombus-else]))

(provide match)

(define-syntax match
  (expression-transformer
   #'match
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       #:literals (rhombus-else)
       [(form-id in ... ((~and alts-tag alts)
                         (block (group bind ...
                                       (~and rhs (block . _))))
                         ...
                         (block (group rhombus-else
                                       (~and else-rhs (block . _)))))
                 . tail)
        #:with (b::binding ...) #'((group bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   (map list (syntax->list #'(b ... ignored)))
                                   (map list (syntax->list #`(b.expanded ... #,(binding-form
                                                                                #'()
                                                                                #'(lambda (v) #t)
                                                                                #'(begin)))))
                                   (syntax->list #'(rhs ... else-rhs))
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]
       [(form-id in ... ((~and alts-tag alts)
                         (block (group bind ...
                                       (~and rhs (block . _))))
                         ...)
                 . tail)
        #:with (b::binding ...) #'((group bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   (map list (syntax->list #'(b ...)))
                                   (map list (syntax->list #'(b.expanded ...)))
                                   (syntax->list #'(rhs ...))
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]))))
