#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     racket/list
                     "srcloc.rkt")
         "expression.rkt"
         "repetition.rkt"
         "parse.rkt"
         "parens.rkt"
         "static-info.rkt"
         "compound-repetition.rkt")

(provide (rename-out [rhombus-block block])
         (for-space rhombus/repet
                    (rename-out [rhombus-block block])))

(define-syntax rhombus-block
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (tag::block form ...))
        (values
         (relocate+reraw
          (respan stx)
          #'(let ()
              (rhombus-body-at tag form ...)))
         #'())]))))

(define-repetition-syntax rhombus-block
  (repetition-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(form-id (~and blk (_::block)))
        (raise-syntax-error #f
                            "block has no repetitions"
                            stx
                            #'blk)]
       [(form-id (_::block form ...))
        #:with (r::repetition ...) #'(form ...)
        (values
         (build-compound-repetition
          stx
          (syntax->list #'(r.parsed ...))
          #:element-statinfo? #t
          (lambda es
            (values #`(begin . #,(map discard-static-infos es))
                    (extract-static-infos (last es)))))
         #'())]))))
