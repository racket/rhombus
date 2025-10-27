#lang racket/base
(require rhombus/dynamic-require)

(define make-string-port (rhombus-dynamic-require 'rhombus '(Port Input open_string)))
(define p (make-string-port "hello"))
(unless (eq? (read p) 'hello)
  (error "bad port read"))

(define is-int? (rhombus-dynamic-require-predicate 'rhombus 'Int))
(unless (is-int? 10) (error "should be an int"))
(when (is-int? "x") (error "shouldn't be an int"))
(unless (eq? (object-name is-int?) '|is_a Int|)
  (error "wrong name for port predicate"))

(define is-input-port? (rhombus-dynamic-require-predicate 'rhombus '(Port Input)))
(unless (is-input-port? (current-input-port)) (error "should be an input port"))
(when (is-input-port? "x") (error "shouldn't be an input port"))
(unless (eq? (object-name is-input-port?) '|is_a Port.Input|)
  (error "wrong name for port predicate"))
