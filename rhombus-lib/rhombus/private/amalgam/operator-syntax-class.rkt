#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         (submod "syntax-class-primitive.rkt" for-syntax-class-syntax)
         (submod "syntax-class-primitive.rkt" for-syntax-class)
         "pack.rkt"
         "name-start-syntax-class.rkt")

(provide (for-syntax make-syntax-class)
         define-operator-syntax-classes
         define-transformer-syntax-class
         define-syntax-class-syntax)

(begin-for-syntax
  (define name-start-fields
    #'((name name #f 0 unpack-term* stx)
       (head #f head tail unpack-tail-list* stx)
       (tail #f tail tail unpack-tail-list* stx))))

(define-syntax (define-operator-syntax-classes stx)
  (syntax-parse stx
    [(_ Parsed (~var :form) parsed-tag
        NameStart:id in-space
        (~optional (~seq AfterPrefixParsed:id (~var :prefix-op+form+tail)
                         AfterInfixParsed:id (~var :infix-op+form+tail)))
        (~optional (~seq #:extra-arity-mask a)
                   #:defaults ([a #'#f])))
     #'(begin
         (define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                               #:kind 'group
                                                               #:arity-mask a
                                                               #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag) stx))
                                                               #:root-swap '(parsed . group)))
         (~? (define-syntax-class-syntax AfterPrefixParsed (make-syntax-class #':prefix-op+form+tail
                                                                              #:kind 'group
                                                                              #:arity-mask (if a (arithmetic-shift a 1) 2)
                                                                              #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag) stx)
                                                                                          (tail #f tail tail unpack-tail-list* stx))
                                                                              #:root-swap '(parsed . group))))
         (~? (define-syntax-class-syntax AfterInfixParsed (make-syntax-class #':infix-op+form+tail
                                                                             #:kind 'group
                                                                             #:arity-mask (if a (arithmetic-shift a 1) 2)
                                                                             #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag) stx)
                                                                                         (tail #f tail tail unpack-tail-list* stx))
                                                                             #:root-swap '(parsed . group))))
         (define-syntax-class-syntax NameStart (make-syntax-class #':name-start
                                                                  #:auto-args #'(in-space)
                                                                  #:kind 'group
                                                                  #:fields name-start-fields)))]))

(define-syntax (define-transformer-syntax-class stx)
  (syntax-parse stx
    [(_ Parsed (~var :form) parsed-tag
        (~optional (~seq #:arity-mask a)
                   #:defaults ([a #'#f])))
     #'(define-syntax-class-syntax Parsed (make-syntax-class #':form
                                                             #:kind 'group
                                                             #:arity-mask a
                                                             #:fields #'((parsed #f parsed 0 (unpack-parsed* 'parsed-tag) stx))
                                                             #:root-swap '(parsed . group)))]))
