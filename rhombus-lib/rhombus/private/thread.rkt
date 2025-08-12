#lang racket/base
(require "version-case.rkt"
         (only-in '#%futures processor-count))

(provide processor-count)

(meta-if-version-at-least
 "8.18.0.11"
 (provide thread
          thread-wait
          parallel-thread-pool?
          make-parallel-thread-pool
          parallel-thread-pool-close)
 (begin
   (require (only-in racket/base
                     [thread thread*]
                     [thread-wait thread-wait*]))
   (provide thread
            thread-wait
            parallel-thread-pool?
            make-parallel-thread-pool
            parallel-thread-pool-close)
   (define (thread thunk
                   #:pool [pool #f]
                   #:keep [keep #f])
     (when keep (error 'thread "keep mode not supported"))
     (thread* thunk))
   (define (thread-wait t fail-k)
     (thread-wait* t))
   (define (parallel-thread-pool? v)
     #f)
   (define (make-parallel-thread-pool [n (processor-count)])
     (error 'make-parallel-thread-pool "not supported"))
   (define (parallel-thread-pool-close p)
     (void))))
