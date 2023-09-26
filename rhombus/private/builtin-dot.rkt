#lang racket/base
(require "mutability.rkt"
         (submod "dot.rkt" for-builtin)
         (submod "map.rkt" for-builtin)
         (submod "set.rkt" for-builtin)
         (submod "cons-list.rkt" for-builtin)
         (submod "array.rkt" for-builtin)
         (submod "box.rkt" for-builtin)
         (submod "syntax-object.rkt" for-builtin)
         (submod "pair.rkt" for-builtin)
         (submod "string.rkt" for-builtin)
         (submod "bytes.rkt" for-builtin)
         (submod "function.rkt" for-builtin)
         (submod "path-object.rkt" for-builtin)
         (submod "srcloc-object.rkt" for-builtin)
         (submod "exn-object.rkt" for-builtin))

(set-builtin->accessor-ref!
 (lambda (v)
   (cond
     [(hash? v) (if (mutable-hash? v)
                    mutable-map-method-table
                    map-method-table)]
     [(set? v) (if (mutable-set? v)
                   mutable-set-method-table
                   set-method-table)]
     [(list? v) cons-list-method-table]
     [(vector? v) array-method-table]
     [(syntax? v) syntax-method-table]
     [(pair? v) pair-method-table]
     [(string? v) string-method-table]
     [(bytes? v) bytes-method-table]
     [(procedure? v) function-method-table]
     [(path? v) path-method-table]
     [(srcloc? v) srcloc-method-table]
     [(exn? v) (get-exn-method-table v)]
     [(box? v) box-method-table]
     [else #f])))

(set-builtin->mutator-ref!
 (lambda (v)
   (cond
     [(box? v) box-mutator-method-table]
     [else #f])))
