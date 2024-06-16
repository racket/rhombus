#lang racket/base
(require (for-syntax racket/base)
         "key-comp.rkt"
         (submod "map.rkt" for-key-comp)
         (submod "set.rkt" for-key-comp))

(provide (for-space rhombus/key_comp
                    ==
                    ===
                    is_now
                    is_same_number_or_object))

(define-key-comp-syntax ==
  (key-comp-maker
   (lambda ()
     (key-comp '== #'equal-always-hash?
               #'Map-build #'Map-pair-build #'for/hashalw
               #'mutable-equal-always-hash? #'MutableMap-build
               #'weak-mutable-equal-always-hash? #'WeakMutableMap-build
               #'#hashalw()
               #'immutable-equal-always-set?
               #'Set-build #'Set-build* #'for/setalw
               #'mutable-equal-always-set? #'MutableSet-build
               #'weak-mutable-equal-always-set? #'WeakMutableSet-build))))

(define-key-comp-syntax ===
  (key-comp-maker
   (lambda ()
     (key-comp '=== #'object-hash?
               #'ObjectMap-build #'ObjectMap-pair-build #'for/hasheq
               #'mutable-object-hash? #'MutableObjectMap-build
               #'weak-mutable-object-hash? #'WeakMutableObjectMap-build
               #'#hasheq()
               #'immutable-object-set?
               #'ObjectSet-build #'ObjectSet-build* #'for/seteq
               #'mutable-object-set? #'MutableObjectSet-build
               #'weak-mutable-object-set? #'WeakMutableObjectSet-build))))

(define-key-comp-syntax is_now
  (key-comp-maker
   (lambda ()
     (key-comp 'is_now #'now-hash?
               #'NowMap-build #'NowMap-pair-build #'for/hash
               #'mutable-now-hash? #'MutableNowMap-build 
               #'weak-mutable-now-hash? #'WeakMutableNowMap-build 
               #'#hash()
               #'immutable-now-set?
               #'NowSet-build #'NowSet-build* #'for/set
               #'mutable-now-set? #'MutableNowSet-build
               #'weak-mutable-now-set? #'WeakMutableNowSet-build))))

(define-key-comp-syntax is_same_number_or_object
  (key-comp-maker
   (lambda ()
     (key-comp 'is_same_number_or_object #'number-or-object-hash?
               #'NumberOrObjectMap-build #'NumberOrObjectMap-pair-build #'for/hasheqv
               #'mutable-number-or-object-hash? #'MutableNumberOrObjectMap-build 
               #'weak-mutable-number-or-object-hash? #'WeakMutableNumberOrObjectMap-build 
               #'#hasheqv()
               #'immutable-number-or-object-set?
               #'NumberOrObjectSet-build #'NumberOrObjectSet-build* #'for/seteqv
               #'mutable-number-or-object-set? #'MutableNumberOrObjectSet-build
               #'weak-mutable-number-or-object-set? #'WeakMutableNumberOrObjectSet-build))))
