#lang racket/base
(require (for-syntax racket/base)
         racket/treelist
         "../version-case.rkt"
         "provide.rkt"
         "parse.rkt"
         "pack.rkt"
         "define-arity.rkt"
         "static-info.rkt"
         (submod "annotation.rkt" for-class)
         "name-root.rkt"
         (submod "module-path-object.rkt" for-primitive)
         (submod "parameter.rkt" for-info)
         "rhombus-primitive.rkt"
         "rename-parameter.rkt")

(provide (for-spaces (#f
                      rhombus/statinfo)
                     (rename-out
                      [rhombus-eval eval]))
         (for-spaces (rhombus/annot
                      rhombus/namespace)
                     Evaluator))

(define-annotation-syntax Evaluator (identifier-annotation namespace? ()))

(define-name-root Evaluator
  #:fields
  (make_rhombus_empty
   make_rhombus
   [current current-namespace]
   [current_print Evaluator.current_print]
   import
   instantiate
   module_is_declared))

(define/arity (make_rhombus_empty)
  (define this-ns (variable-reference->empty-namespace (#%variable-reference)))
  (define ns (make-base-empty-namespace))
  (namespace-attach-module this-ns
                           '(lib "rhombus/main.rhm")
                           ns)
  ns)

(define/arity (make_rhombus)
  (define ns (make_rhombus_empty))
  (parameterize ([current-namespace ns])
    (namespace-require '(lib "rhombus/main.rhm")))
  ns)

(define/arity #:name eval (rhombus-eval e
                                        #:as_interaction [as_interaction #f])
  (unless (syntax? e)
    (raise-annotation-failure who e "Syntax"))
  (if as_interaction
      (eval `(#%top-interaction . ,#`(multi #,@(unpack-multi e 'eval #f))))
      (eval #`(rhombus-top #,@(unpack-multi e 'eval #f)))))

(define-static-info-syntaxes (current-namespace)
  #:getter get-parameter-static-infos)

(define (check-module-path who mod-path)
  (unless (module-path? mod-path)
    (raise-annotation-failure who mod-path "ModulePath")))

(define/arity (import mod-path)
  (check-module-path who mod-path)
  (namespace-require (module-path-s-exp mod-path)))

(define/arity (instantiate mod-path [names #f])
  (check-module-path who mod-path)
  (unless (or (not names)
              (symbol? names)
              (and (treelist? names)
                   (not (treelist-empty? names))
                   (for/and ([n (in-treelist names)])
                     (symbol? n))))
    (raise-annotation-failure who names "maybe(Symbol || NonemptyList.of(Symbol))"))
  (define name (if (and (treelist? names)
                        (= 1 (treelist-length names)))
                   (treelist-ref names 0)
                   names))
  (define mod (module-path-s-exp-or-index-or-resolved mod-path))
  (define (via-eval)
    (define ns (variable-reference->empty-namespace
                (eval #'(#%variable-reference))))
    (namespace-require (module-path-s-exp mod-path))
    (eval (namespace-syntax-introduce
           #`(rhombus-top (group #,@(datum->syntax
                                     #f
                                     (if (treelist? name)
                                         (cons
                                          (treelist-ref name 0)
                                          (apply
                                           append
                                           (for/list ([n (in-treelist (treelist-rest name))])
                                             (list '(op |.|) n))))
                                         (list name))))))))
  (if (treelist? name)
      (via-eval)
      (meta-if-version-at-least
       "8.16.0.3"
       (dynamic-require mod name 'error via-eval)
       (with-handlers ([exn:fail? (lambda (x) (via-eval))])
         (dynamic-require mod name)))))

(define/arity (module_is_declared mod-path
                                  #:load [load? #f])
  (check-module-path who mod-path)
  (module-declared? (module-path-s-exp-or-index-or-resolved mod-path) load?))

(define Evaluator.current_print (rename-parameter current-print 'Evaluator.current_print))
(set-primitive-who! 'current-print 'Evaluator.current_print)
