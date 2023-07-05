#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "provide.rkt"
         "name-root.rkt"
         "definition.rkt"
         "expression.rkt"
         "space.rkt"
         (submod "class.rkt" for-together)
         "interface.rkt"
         (submod "interface.rkt" for-together)
         (only-in "class-together-parse.rkt"
                  rhombus-together)
         "forwarding-sequence.rkt"
         "parse.rkt"
         "parens.rkt")

(provide (for-spaces (rhombus/namespace
                      rhombus/space
                      rhombus/defn)
                     class))

(define-space-syntax class
  (space-syntax rhombus/class))
  
(define-name-root class
  #:fields
  (together))

(define-defn-syntax class class-transformer)

(define-defn-syntax together
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ (_::block defn ...))
         (with-syntax ([(defn ...)
                        (for/list ([defn (in-list (syntax->list #'(defn ...)))])
                          (syntax-parse defn
                            #:datum-literals (group block)
                            [((~and tag group) id . rest)
                             #:when (free-identifier=? (in-defn-space #'id) (in-defn-space #'class))
                             #`(tag #,(datum->syntax #'here 'class_for_together #'id #'id) . rest)]
                            [((~and tag group) id . rest)
                             #:when (free-identifier=? (in-defn-space #'id) (in-defn-space #'interface))
                             #`(tag #,(datum->syntax #'here 'interface_for_together #'id #'id) . rest)]
                            [_
                             (raise-syntax-error #f
                                                 "not a class or interface form"
                                                 stx
                                                 defn)]))])
           #'((rhombus-mixed-forwarding-sequence (together-finish) rhombus-together
                                                 (rhombus-definition defn) ...)))]))))

(define-syntax (together-finish stx)
  (syntax-parse stx
    #:literals (begin)
    [(_ [(_ (begin defn ... last-defn)) _] ...)
     #`(begin
         defn ... ...
         last-defn ...)]))
