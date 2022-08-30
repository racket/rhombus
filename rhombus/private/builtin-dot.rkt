#lang racket

(require (submod "dot.rkt" for-builtin)
         (submod "map.rkt" for-builtin)
         (submod "set.rkt" for-builtin)
         (submod "list.rkt" for-builtin)
         (submod "array.rkt" for-builtin)
         (submod "syntax-object.rkt" for-builtin))

(set-builtin->accessor-ref!
 (lambda (v)
   (cond
     [(hash? v) map-method-table]
     [(set? v) set-method-table]
     [(list? v) list-method-table]
     [(vector? v) array-method-table]
     [(syntax? v) syntax-method-table]
     [else #f])))
