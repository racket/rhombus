#lang racket/base

(require racket/runtime-config
         shrubbery/parse)

(current-read-interaction
 (lambda (src in)
   ;; For now, read until EOF or an unindented ";" on its own line
   (define-values (i o) (make-pipe))
   (let loop ()
     (define l (read-line in))
     (unless (eof-object? l)
       (displayln l o)
       (unless (string=? l ";")
         (loop))))
   (close-output-port o)
   (port-count-lines! i)
   (define r (parse-all i #:source src))
   r))
