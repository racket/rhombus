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
       (obs-observe! enabled-obs (lambda (v) (enable v)))]
      [(not enabled-obs)
       (log-error "dis")
       (enable #f)])))

