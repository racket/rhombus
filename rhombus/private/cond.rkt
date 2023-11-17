#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "parse.rkt"
         "else-clause.rkt"
         "parens.rkt"
         "realm.rkt")

(provide (rename-out [rhombus-if if]
                     [rhombus-cond cond]
                     [rhombus-when when]
                     [rhombus-unless unless]))

(define-syntax rhombus-if
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       [(form-id test ... (_::alts alt ...))
        (syntax-parse #'(alt ...)
          [((tag-thn::block thn ...)
            (tag-els::block els ...))
           (values
            (relocate+reraw
             (respan stx)
             #'(if (rhombus-expression (group test ...))
                   (rhombus-body-at tag-thn thn ...)
                   (rhombus-body-at tag-els els ...)))
            #'())]
          [_
           (raise-syntax-error #f
                               "expected two alternatives"
                               stx)])]))))

(define-syntax rhombus-cond
  (expression-transformer
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group block)
       [(form-id (_::alts
                  (_::block (group pred ... ((~and tag block) rhs ...)))
                  ...
                  e::else-clause))
        (values
         (relocate+reraw
          (respan stx)
          #'(cond
              [(rhombus-expression (group pred ...))
               (rhombus-body-at tag rhs ...)]
              ...
              [else e.parsed]))
         #'())]
       [(form-id (_::alts
                  (_::block (group pred ... ((~and tag block) rhs ...)))
                  ...))
        (values
         (relocate+reraw
          (respan stx)
          #'(cond
              [(rhombus-expression (group pred ...))
               (rhombus-body-at tag rhs ...)]
              ...
              [else (cond-fallthrough 'form-id)]))
         #'())]
       [(form-id (_::block))
        (values
         (relocate+reraw
          (respan stx)
          #'(cond-fallthrough 'form-id))
         #'())]))))

(define (cond-fallthrough who)
  (raise-arguments-error* who rhombus-realm "no matching case"))

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
    [(form-id test ... (_::alts alt ...))
     (syntax-parse #'(alt ...)
       [((tag-thn::block thn ...))
        (values
         (relocate+reraw
          (respan stx)
          #`(#,racket-form-id (rhombus-expression (group test ...))
             (rhombus-body-at tag-thn thn ...)))
         #'())]
       [_
        (raise-syntax-error #f
                            "expected a single alternative"
                            stx)])]))
