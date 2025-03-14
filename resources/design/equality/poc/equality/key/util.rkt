#lang racket

(provide make-memoizer)

(define (make-memoizer func)
  (define semaphore (make-semaphore 1))
  (define results (make-ephemeron-hash))
  (lambda (arg)
    ; First, we try to get the memoized result without locking.
    (hash-ref results arg
      (thunk
        ; If that didn't work, we lock.
        (call-with-semaphore semaphore
          (thunk
            ; Now, we try again to get the memoized result just in case it
            ; showed up while we were waiting for the lock. If it didn't, we
            ; compute it ourselves and store it for future use.
            ;
            (hash-ref! results arg (thunk (func arg)))))))))
