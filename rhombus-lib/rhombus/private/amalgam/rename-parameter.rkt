#lang racket/base
(require "../version-case.rkt"
         "realm.rkt")

(provide rename-parameter)

(meta-if-version-at-least
 "8.15.0.4"
 (define (rename-parameter param name)
   (make-derived-parameter param
                           values
                           values
                           name
                           rhombus-realm))
 (define (rename-parameter param name)
   param))
