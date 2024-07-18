#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "parse.rkt")

(provide define-class-body-step)

(define-syntax-rule (define-class-body-step class-body-step
                      :class-clause
                      class-expand-data
                      class-clause-accum)
  (...
   (define-syntax class-body-step
     (lambda (stx)
       ;; parse the first form as a class clause, if possible, otherwise assume
       ;; an expression or definition
       (syntax-parse stx
         [(_ (data accum) form . rest)
          #:with (~var clause (:class-clause (class-expand-data #'data #'accum))) (syntax-local-introduce #'form)
          (syntax-parse (syntax-local-introduce #'clause.parsed)
            #:datum-literals (group parsed)
            [((group (parsed #:rhombus/class_clause p)) ...)
             #:with (new-accum ...) (class-clause-accum #'(p ...))
             #`(begin p ... (class-body-step (data (new-accum ... . accum)) . rest))]
            [(form ...)
             #`(class-body-step (data accum) form ... . rest)])]
         [(_ data+accum form . rest)
          #`(rhombus-top-step
             class-body-step
             #f
             (data+accum)
             form . rest)]
         [(_ data+accum) #'(begin)])))))
