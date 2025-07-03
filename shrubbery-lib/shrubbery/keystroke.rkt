#lang racket/base
(require "variant.rkt"
         "armor.rkt")

(provide shrubbery-keystrokes
         make-shrubbery-keystrokes)

(define (map-meta key proc)
  (append
   (if #t ; mapping option as meta?
       (list (list (format "?:a:~a" key) proc))
       null)
   (list (list (format "?:m:~a" key) proc)
         (list (format "esc;~a" key) proc))))

(define shrubbery-keystrokes
  (append
   (map-meta "a" toggle-armor)))

(define (make-shrubbery-keystrokes #:variant [variant default-variant])
  shrubbery-keystrokes)
