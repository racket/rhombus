#lang racket/base
(require (except-in scribble/rhombus
                    if)
         (prefix-in manual: scribble/manual)
         "../private/doc.rhm"
         "../private/docmodule.rhm"
         (only-in "../private/typeset-doc.rkt"
                  grammar))

(provide (all-from-out scribble/rhombus)
         litchar
         (rename-out [manual:deftech deftech]
                     [manual:tech tech]
                     [manual:math math]
                     [manual:filepath filepath])
         doc
         docmodule
         grammar)

(module reader syntax/module-reader
  #:language 'scribble/rhombus/manual
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require (submod scribble/rhombus reader)))

(define (litchar ls)
  (manual:litchar (if (string? ls) ls (car ls))))

