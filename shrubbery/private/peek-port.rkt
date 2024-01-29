#lang racket/base

(provide call-with-peeking-port)

(struct peek-port (in [pos #:mutable]))

(define current-peek-port-key (gensym 'current-peek-port))
(define (current-peek-port) (continuation-mark-set-first #f current-peek-port-key))

;; make the port once, to minimize overhead:
(define peek-input-port
  (make-input-port
   'peeker
   (lambda (bstr)
     (define pp (current-peek-port))
     (define pos (peek-port-pos pp))
     (define n (peek-bytes! bstr pos (peek-port-in pp)))
     (cond
       [(eof-object? n) eof]
       [else
        (set-peek-port-pos! pp (+ pos n))
        n]))
   (lambda (bstr offset evt)
     (define pp (current-peek-port))
     (peek-bytes! bstr (+ offset (peek-port-pos pp)) (peek-port-in pp)))
   void))

(define (call-with-peeking-port in proc)
  (with-continuation-mark
   current-peek-port-key
   (peek-port in 0)
   (proc peek-input-port)))
