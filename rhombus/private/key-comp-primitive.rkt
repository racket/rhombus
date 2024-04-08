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
  (key-comp '== #'equal-always-hash?
            #'Map-build #'Map-pair-build #'list->map
            #'mutable-equal-always-hash? #'MutableMap-build
            #'#hashalw()
            #'immutable-equal-always-set?
            #'Set-build #'Set-build* #'list->set
            #'mutable-equal-always-set? #'MutableSet-build))

(define-key-comp-syntax ===
  (key-comp '=== #'object-hash?
            #'ObjectMap-build #'ObjectMap-pair-build #'list->object-map
            #'mutable-object-hash? #'MutableObjectMap-build
            #'#hasheq()
            #'immutable-object-set?
            #'ObjectSet-build #'ObjectSet-build* #'list->object-set
            #'mutable-object-set? #'MutableObjectSet-build))

(define-key-comp-syntax is_now
  (key-comp 'is_now #'now-hash?
            #'NowMap-build #'NowMap-pair-build #'list->now-map
            #'mutable-now-hash? #'MutableNowMap-build 
            #'#hash()
            #'immutable-now-set?
            #'NowSet-build #'NowSet-build* #'list->now-set
            #'mutable-now-set? #'MutableNowSet-build))

(define-key-comp-syntax is_same_number_or_object
  (key-comp 'is_same_number_or_object #'number-or-object-hash?
            #'NumberOrObjectMap-build #'NumberOrObjectMap-pair-build #'list->number-or-object-map
            #'mutable-number-or-object-hash? #'MutableNumberOrObjectMap-build 
            #'#hasheqv()
            #'immutable-number-or-object-set?
            #'NumberOrObjectSet-build #'NumberOrObjectSet-build* #'list->number-or-object-set
            #'mutable-number-or-object-set? #'MutableNumberOrObjectSet-build))
