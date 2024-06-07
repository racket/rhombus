#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "placeholder.rkt")

(provide (for-spaces (#f
                      rhombus/bind)
                     &
                     ~&))

(define-placeholder-syntax &
  "misuse outside of a call or a constructor"
  "misuse outside of a function formal argument or constructor pattern")
  
(define-placeholder-syntax ~&
  "misuse outside of a call or a constructor"
  "misuse outside of a function formal argument or constructor pattern")
