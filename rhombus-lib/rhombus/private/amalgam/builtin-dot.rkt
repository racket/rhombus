#lang racket/base
(require "mutability.rkt"
         "treelist.rkt"
         "mutable-treelist.rkt"
         (submod "dot.rkt" for-builtin)
         (submod "map.rkt" for-builtin)
         (submod "set.rkt" for-builtin)
         (submod "list.rkt" for-builtin)
         (submod "array.rkt" for-builtin)
         (submod "box.rkt" for-builtin)
         (submod "syntax-object.rkt" for-builtin)
         (submod "pair.rkt" for-builtin)
         (submod "string.rkt" for-builtin)
         (submod "bytes.rkt" for-builtin)
         (submod "char.rkt" for-builtin)
         (submod "function.rkt" for-builtin)
         (submod "path-object.rkt" for-builtin)
         (submod "srcloc-object.rkt" for-builtin)
         (submod "port.rkt" for-builtin)
         (submod "exn-object.rkt" for-builtin))

(define (builtin->accessor-ref v)
  (cond
    [(hash? v) (if (mutable-hash? v)
                   mutable-map-method-table
                   map-method-table)]
    [(set? v) (if (mutable-set? v)
                  mutable-set-method-table
                  set-method-table)]
    [(treelist? v) treelist-method-table]
    [(list? v) list-method-table]
    [(vector? v) array-method-table]
    [(syntax? v) syntax-method-table]
    [(pair? v) pair-method-table]
    [(string? v) string-method-table]
    [(bytes? v) bytes-method-table]
    [(char? v) char-method-table]
    [(procedure? v) function-method-table]
    [(path? v) path-method-table]
    [(srcloc? v) srcloc-method-table]
    [(exn? v) (get-exn-method-table v)]
    [(input-port? v) input-port-method-table]
    [(output-port? v) (if (string-port? v)
                          output-string-port-method-table
                          output-port-method-table)]
    [(box? v) box-method-table]
    [(mutable-treelist? v) mutable-treelist-method-table]
    [else #f]))

(define (builtin->mutator-ref v)
  (cond
    [(box? v) box-mutator-method-table]
    [else #f]))

(void (set-builtin->accessor-ref! builtin->accessor-ref))
(void (set-builtin->mutator-ref! builtin->mutator-ref))
