#lang racket/base
(require syntax/parse/pre
         (for-template "parens.rkt")
         "namespace-options-block.rkt")

(provide :class-options-block)

(define-splicing-syntax-class :class-options-block
  #:attributes ([form 1] name doc)
  #:datum-literals (group)
  (pattern (~seq)
           #:with (form ...) '()
           #:with name #'#f
           #:with doc #'#f)
  (pattern (~seq (_::block
                  (~and name-g (group #:name . _))
                  (~and doc-g (group #:doc . _))
                  c-form
                  ...))
           #:cut
           #:with (::options-block) #'((block name-g c-form ...))
           #:with doc #'doc-g)
  (pattern (~seq (_::block
                  (~and doc-g (group #:doc . _))
                  c-form
                  ...))
           #:cut
           #:with (::options-block) #'((block c-form ...))
           #:with doc #'doc-g)
  (pattern (~seq (_::block c-form ...))
           #:with (::options-block) #'((block c-form ...))
           #:with doc #'#f))
