#lang racket/base
(require rhombus/private/version-case)

(define src (collection-file-path "amalgam.rkt" "rhombus/private"))
(define exp
  (parameterize ([read-accept-reader #t])
    (call-with-input-file* src read)))

(define no-demod-mode?
  (let loop ([exp exp])
    (cond
      [(eq? exp '#:no-demod)
       #t]
      [(pair? exp)
       (or (loop (car exp))
           (loop (cdr exp)))])))

(unless no-demod-mode?
  (error "amalgam is not in `#:no-demod` mode"))
#;
(meta-if-version-at-least
 "8.14.0.3"
 (when no-demod-mode?
   (error "amalgam is in `#:no-demod` mode"))
 (void))
