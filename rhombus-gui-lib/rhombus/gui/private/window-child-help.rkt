#lang racket/base
(require racket/class
         racket/gui/base
         racket/gui/easy
         rhombus/parse
         (only-in rhombus
                  #%call)
         "window-callback.rhm"
         (only-in (submod "view.rhm" private)
                  get_gui_handle_view)
         (only-in (submod "event.rhm" private)
                  _KeyEvent
                  _MouseEvent))

(provide add_window_child_callbacks
         mix_window_child_callbacks)

(define-syntax-rule (get cb field)
  (rhombus-expression (group WindowCallbacks (op |.|) field (parens (group cb)))))

(define (add_window_child_callbacks % cb)
  (if cb
      (class %
        (inherit accept-drop-files)
        (define/override (on-drop-file path)
          ((get cb drop_file) path)
          (void))
        (define/override (on-move x y)
          ((get cb move) x y)
          (void))
        (define/override (on-size w h)
          ((get cb size) w h)
          (void))
        (define/override (on-focus on?)
          ((get cb focus) on?)
          (void))
        (define/override (on-subwindow-char w ev)
          (define v (get_gui_handle_view w))
          (when v
            ((get cb sub_key) v (rhombus-expression (group _KeyEvent (parens (group ev)) (parens))))))
        (define/override (on-subwindow-event w ev)
          (define v (get_gui_handle_view w))
          (and v
               ((get cb sub_mouse) v (rhombus-expression (group _MouseEvent (parens (group ev)) (parens))))))
        (define/override (on-subwindow-focus w on?)
          (define v (get_gui_handle_view w))
          (and v
               ((get cb sub_focus) v on?)))
        (define/override (on-superwindow-activate on?)
          ((get cb super_activate) on?)
          (void))
        (define/override (on-superwindow-enable on?)
          ((get cb super_enable) on?)
          (void))
        (define/override (on-superwindow-show on?)
          ((get cb super_show) on?)
          (void))
        (super-new)
        (let ([accepts-drop (get cb accepts_drop_file)])
          (cond
            [(obs? accepts-drop)
             (obs-observe! accepts-drop (lambda (v) (accept-drop-files v)))]
            [accepts-drop
             (accept-drop-files #true)])))
      %))

(define (mix_window_child_callbacks mixin cb)
  (lambda (%)
    (mixin (add_window_child_callbacks % cb))))
