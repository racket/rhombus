#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "name-root.rkt"
         "definition.rkt"
         "space.rkt"
         "definition+space.rkt"
         (submod "class.rkt" for-together)
         (submod "interface.rkt" for-together)
         (only-in "class-together-parse.rkt"
                  rhombus-together)
         "forwarding-sequence.rkt"
         "parse.rkt"
         "parens.rkt")

(provide class)

(define-name-root class
  #:root
  (space+definition-transformer
   (space-syntax rhombus/class)
   class-transformer)
  #:fields
  (together))

(define-syntax together
  (definition-transformer
    (lambda (stx)
      (syntax-parse stx
        [(_ (_::block defn ...))
         (with-syntax ([(defn ...)
                        (for/list ([defn (in-list (syntax->list #'(defn ...)))])
                          (syntax-parse defn
                            #:datum-literals (group block)
                            #:literals (class interface)
                            [((~and tag group) (~and id class) . rest)
                             #`(tag #,(datum->syntax #'here 'class_for_together #'id #'id) . rest)]
                            [((~and tag group) (~and id interface) . rest)
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
    [(_ (_ (begin defn ... last-defn)) ...)
     #`(begin
         defn ... ...
         last-defn ...)]))

                  
              
