#lang racket/base
(require (rename-in scribble/rhombus
                    [if rhombus:if]
                    [module rhombus:module])
         (prefix-in manual: scribble/manual)
         "../private/doc.rhm"
         "../private/docmodule.rhm"
         "../private/example.rhm"
         "../private/rhombus-doc.rkt"
         "../private/rhombus-spacer.rhm")

(provide (all-from-out scribble/rhombus
                       "../private/rhombus-doc.rkt")
         (rename-out [rhombus:if if]
                     [rhombus:module module])
         litchar
         (rename-out [manual:deftech deftech]
                     [manual:tech tech]
                     [manual:math math]
                     [manual:filepath filepath]
                     [manual:exec exec]
                     [manual:hash-lang hash_lang]
                     [manual:onscreen onscreen])
         doc
         (for-space rhombus/defn
                    nonterminal)
         (for-space rhombus/decl
                    docmodule)
         rhombusmodname
         rhombuslangname
         racketmodname
         examples
         make_rhombus_eval
         close_eval
         (all-from-out "../private/rhombus-spacer.rhm"))

(module reader syntax/module-reader
  #:language 'scribble/rhombus/manual
  #:read read-proc
  #:read-syntax read-syntax-proc
  #:info get-info-proc
  #:whole-body-readers? #t
  (require (submod scribble/rhombus reader)))

(define (litchar ls)
  (manual:litchar (if (string? ls) ls (car ls))))
