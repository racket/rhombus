#lang racket/base
(require (for-syntax racket/base)
         "implicit.rkt"
         "dot.rkt"
         "parse.rkt"
         "dynamic-static.rkt")

(begin-for-syntax
  (provide dynamic_call_name
           dynamic_dot_name
           dynamic_index_name)

  (define dynamic_call_name #'#%call)
  (define dynamic_dot_name #'|.|)
  (define dynamic_index_name #'#%index))

(define-syntax-rule (static)
  (begin
    (rhombus-definition (group use_static)) ;; defines `#%dynamism`
    (begin-for-syntax
      (provide static_call_name
               static_dot_name
               static_index_name)

      (define static_call_name #'#%call)
      (define static_dot_name #'|.|)
      (define static_index_name #'#%index))))

(static)
