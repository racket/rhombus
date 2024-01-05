#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "reducer.rkt")

(provide (for-space rhombus/reducer
                    all
                    any))

(define-reducer-syntax all
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer
                 #'build-result
                 #'([result #true])
                 #f
                 #'build-accum
                 #f
                 #'build-stop-false
                 #'build-accum-result
                 #'()
                 #'elem)
                #'tail)]))))

(define-reducer-syntax any
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ . tail)
        (values (reducer
                 #'build-result
                 #'([result #f])
                 #f
                 #'build-accum
                 #f
                 #'build-stop-true
                 #'build-accum-result
                 #'()
                 #'elem)
                #'tail)]))))

(define-syntax (build-result stx)
  (syntax-parse stx
    [(_ _ e) #'e]))

(define-syntax (build-accum stx)
  (syntax-parse stx
    [(_ elem e) #'(define elem e)]))

(define-syntax (build-stop-false stx)
  (syntax-parse stx
    [(_ elem) #'(not elem)]))

(define-syntax (build-stop-true stx)
  (syntax-parse stx
    [(_ elem) #'elem]))

(define-syntax (build-accum-result stx)
  (syntax-parse stx
    [(_ elem) #'elem]))
