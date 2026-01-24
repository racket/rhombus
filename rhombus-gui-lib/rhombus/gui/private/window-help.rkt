#lang racket/base
(require racket/class
         racket/gui/base
         racket/gui/easy)

(provide add_window_callbacks)

(define (add_window_callbacks % enabled-obs)
  (class %
    (inherit enable)
    (super-new)
    (cond
      [(obs? enabled-obs)
       (obs-observe! enabled-obs (lambda (v) (enable v)))
       (unless (obs-peek enabled-obs)
         (enable #f))]
      [(not enabled-obs)
       (enable #f)])))
