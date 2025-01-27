#lang racket/base

(module reader racket/base
  (require "private/lang.rkt")

  (provide (rename-out [shrubbery-read read]
                       [shrubbery-read-syntax read-syntax])
           get-info
           get-info-proc)

  (define (shrubbery-read in)
    (syntax->datum
     (shrubbery-read-syntax #f in)))

  (define (shrubbery-read-syntax src in)
    (shrubbery-read-syntax/mode src in
                                #:mode 'text))

  (define (get-info in mod line col pos)
    (lambda (key default)
      (get-info-proc key default (lambda (key default) default))))

  (define (get-info-proc key default make-default)
    (shrubbery-get-info-proc/mode key default make-default
                                  #:mode 'text)))
