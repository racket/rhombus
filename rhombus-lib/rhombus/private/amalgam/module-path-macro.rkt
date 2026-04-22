#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "name-root.rkt"
                     (submod "syntax-class-primitive.rkt" for-syntax-class))
         (only-in "space.rkt" space-syntax)
         "space-provide.rkt"
         (submod "module-path.rkt" for-import-export)
         (submod "module-path.rkt" for-meta))

(define+provide-space module_path rhombus/modpath
  #:fields
  ())

(provide
 (for-syntax (for-space rhombus/namespace
                        module_path_meta)))

(begin-for-syntax
  (define-name-root module_path_meta
    #:fields
    (space
     Parsed
     NameStart)))

(define-for-syntax space
  (space-syntax rhombus/modpath))

(begin-for-syntax
  (define-operator-syntax-classes
    Parsed :module-path #:rhombus/modpath
    NameStart in-module-path-space))
