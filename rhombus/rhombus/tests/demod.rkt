#lang racket/base

(define src (collection-file-path "amalgam.rkt" "rhombus/private"))
(define exp
  (parameterize ([read-accept-reader #t])
    (call-with-input-file* src read)))

(define no-demod-mode?
  (let loop ([exp exp])
    (or (eq? exp '#:no-demod)
        (and (pair? exp)
             (or (loop (car exp))
                 (loop (cdr exp)))))))

#;
(unless no-demod-mode?
  (error "amalgam is not in `#:no-demod` mode"))
(when no-demod-mode?
  (error "amalgam is in `#:no-demod` mode"))
