#lang racket/base
(require (rename-in scribble/rhombus
                    [if rhombus:if]
                    [module rhombus:module])
         (prefix-in manual: scribble/manual)
         "../private/rhombus.rhm"
         "../private/rhombus_typeset.rhm"
         "../private/doc.rhm"
         "../private/docmodule.rhm"
         "../private/example.rhm"
         "../private/rhombus-doc.rkt"
         "../private/rhombus-spacer.rhm"
         "../private/manual-text.rhm")

(provide (all-from-out scribble/rhombus
                       "../private/rhombus.rhm"
                       "../private/rhombus_typeset.rhm"
                       "../private/rhombus-doc.rkt"
                       "../private/manual-text.rhm")
         (rename-out [rhombus:if if])
         (for-space rhombus/decl
                    (rename-out
                     [rhombus:module module]))
         doc
         (for-space rhombus/defn
                    nonterminal)
         nontermref
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

(module configure-expand racket/base
  (require rhombus/expand-config)
  (provide enter-parameterization
           exit-parameterization))
