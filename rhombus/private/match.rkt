#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "expression.rkt"
         "binding.rkt"
         "parse.rkt"
         "function.rkt")

(provide match)

(define-syntax match
  (expression-transformer
   #'match
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (alts block group)
       [(form-id in ... ((~and alts-tag alts)
                         (block (group bind ...
                                       (~and rhs (block . _))))
                         ...
                         (block (group #:else
                                       (~and else-rhs (block . _)))))
                 . tail)
        #:with (b::binding ...) #'((group bind ...) ...)
        (values
         #`(#,(build-case-function #'match
                                   #'((b) ... (ignored))
                                   #`((b.parsed) ... (#,(binding-form
                                                         #'()
                                                         #'(lambda (v) #t)
                                                         #'(begin))))
                                   #'(rhs ... else-rhs)
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
                                   #'((b) ...)
                                   #'((b.parsed) ...)
                                   #'(rhs ...)
                                   #'form-id #'alts-tag)
            (rhombus-expression (group in ...)))
         #'tail)]))))
