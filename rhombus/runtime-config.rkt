#lang racket/base

(require racket/runtime-config
         racket/port
         racket/interaction-info
         shrubbery/parse
         shrubbery/print
         "private/set.rkt"
         (prefix-in rhombus: "private/print.rkt")
         (submod "private/print.rkt" redirect))

(current-interaction-info '#((submod rhombus reader)
                             get-interaction-info
                             #f))

(current-read-interaction
 (lambda (src in)
   (when (terminal-port? in)
     (flush-output (current-output-port)))
   (define (ensure-count in)
     (if (port-counts-lines? in)
         in
         (let ([in (dup-input-port in)])
           (port-count-lines! in)
           in)))
   (parse-all (ensure-count in) #:source src #:interactive? #t)))

(print-boolean-long-form #t)

(define orig-print (global-port-print-handler))

(global-port-print-handler
 (lambda (v op [mode 0])
   (if (racket-print-redirect? v)
       (orig-print (racket-print-redirect-val v) op 1)
       (rhombus:print v op))))

(error-syntax->string-handler
 (lambda (s len)
   (shrubbery-syntax->string s #:max-length len)))
