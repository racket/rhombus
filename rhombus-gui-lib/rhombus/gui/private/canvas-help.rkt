#lang racket/base
(require racket/class)

(provide add_canvas_callbacks)

(define (add_canvas_callbacks % on_key on_mouse)
  (class %
    (super-new)
    (define/override (on-char e)
      (on_key e this))
    (define/override (on-event e)
      (on_mouse e this))))
