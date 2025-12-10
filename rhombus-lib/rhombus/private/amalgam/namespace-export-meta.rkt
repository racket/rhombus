#lang racket/base
(require (for-syntax racket/base)
         racket/treelist
         "provide.rkt"
         "class-primitive.rkt"
         "function-arity-key.rkt"
         "index-result-key.rkt"
         "define-arity.rkt"
         (submod "list.rkt" for-compound-repetition)
         (submod "syntax-object.rkt" for-quasiquote))

(provide (for-spaces (rhombus/namespace
                      rhombus/annot)
                     namespace_meta.Exports))

(module+ for-namespace
  (provide namespace-exports
           (for-syntax get-namespace-exports-static-infos)))

(define/method (namespace_meta.Exports.external_names exports)
  #:static-infos ((#%call-result #,(get-treelist-of-syntax-static-infos)))
  ((namespace_meta.Exports.handler exports) 'external_names))

(define/method namespace_meta.Exports.include_spaces
  (lambda (exports)
    ((namespace_meta.Exports.handler exports) 'include_spaces)))

(define/method namespace_meta.Exports.exclude_spaces
  #:static-infos ((#%call-result #,(get-treelist-static-infos)))
  (lambda (exports)
    ((namespace_meta.Exports.handler exports) 'exclude_spaces)))

(define/method namespace_meta.Exports.add
  #:static-infos ((#%call-result #,(get-namespace-exports-static-infos)))
  (lambda (exports external-id internal-id [include-spaces 'all] [exclude-spaces (treelist)])
    (((namespace_meta.Exports.handler exports) 'adder) who external-id internal-id include-spaces exclude-spaces)))

(define/method namespace_meta.Exports.declaration
  #:static-infos ((#%call-result #,(get-syntax-static-infos)))
  (lambda (exports name)
    (((namespace_meta.Exports.handler exports) 'declarer) who name)))

(define-primitive-class namespace_meta.Exports namespace-exports
  #:new
  #:translucent
  #:fields
  ([(handler handler)])
  #:properties
  ()
  #:methods
  (external_names
   include_spaces
   exclude_spaces
   add
   declaration))
