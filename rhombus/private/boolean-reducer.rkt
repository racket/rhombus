#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "annotation-string.rkt")
         "reducer.rkt"
         "parse.rkt"
         "static-info.rkt")

(provide (for-space rhombus/reducer
                    all
                    any))

(define-reducer-syntax all
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        (values (reducer
                 #'build-result
                 #'([result #true])
                 #'build-accum
                 #f
                 #'build-stop-false
                 #'build-accum-result
                 #'()
                 #'elem)
                #'())]))))

(define-reducer-syntax any
  (reducer-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_)
        (values (reducer
                 #'build-result
                 #'([result #f])
                 #'build-accum
                 #f
                 #'build-stop-true
                 #'build-accum-result
                 #'()
                 #'elem)
                #'())]))))

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
