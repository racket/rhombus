#lang racket/base
(require racket/class
         racket/gui/easy
         (submod "view.rhm" private))

(provide mix-wrap)

(define (mix-wrap k)
  (define view
    (k
     (lambda (%)
       (class %
         (super-new)
         (set_view_gui_handle view this)))))
  view)
