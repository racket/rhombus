#lang racket/base
(require racket/match
         (only-in scribble/base tt))

(provide module-path->rhombus-module-path)

(define (module-path->rhombus-module-path s-exp)
  (match s-exp
    [`(lib ,str)
     (cond
       [(regexp-match #rx"^(.+)/main[.]rhm$" str)
        => (lambda (m)
             (tt (cadr m)))]
       [(regexp-match? #rx"[.]rhm$" str)
        (tt (substring str 0 (- (string-length str) 4)))]
       [else
        (tt "lib(" str ")")])]
    [`(submod ,mp ,names ...)
     (cons (module-path->rhombus-module-path mp)
           (for/list ([name (in-list names)])
             (tt "!" (symbol->string name))))]
    [else
     (tt (format "~s" s-exp))]))
