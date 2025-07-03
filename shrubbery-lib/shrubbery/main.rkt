#lang racket/base

(module reader racket/base
  (require "private/lang.rkt"
           "variant.rkt")

  (provide (rename-out [shrubbery-read read]
                       [shrubbery-read-syntax read-syntax])
           get-info
           get-info-proc
           make-get-info-proc)

  (define (shrubbery-read in #:variant [variant default-variant])
    (syntax->datum
     (shrubbery-read-syntax #f in #:variant variant)))

  (define (shrubbery-read-syntax src in #:variant [variant default-variant])
    (shrubbery-read-syntax/mode src in #:variant variant))

  (define (get-info in mod line col pos)
    (lambda (key default)
      (get-info-proc key default (lambda (key default) default))))

  (define (get-info-proc key default make-default #:variant [variant default-variant])
    (shrubbery-get-info-proc/mode key default make-default #:variant variant))

  (define (make-get-info-proc #:variant [variant default-variant])
    (lambda (key default make-default)
      (get-info-proc key default make-default #:variant variant))))
