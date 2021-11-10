#lang racket/base
(require scribble/rhombus
         (prefix-in manual: scribble/manual))
(provide (all-from-out scribble/rhombus)
         litchar
         (rename-out [manual:deftech deftech]))

(module reader syntax/module-reader
  #:language 'scribble/rhombus/manual
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require (submod scribble/rhombus reader)))

(define (litchar ls)
  (manual:litchar (car ls)))

