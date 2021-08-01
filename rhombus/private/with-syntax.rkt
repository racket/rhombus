#lang racket/base
(require syntax/parse)

(provide with-syntax-parse)

(define-syntax with-syntax-parse
  (syntax-rules ()
    [(_ () body ...)
     (let () body ...)]
    [(_ ([pat rhs] . more) . bodys)
     (syntax-parse rhs
       [pat (with-syntax-parse more . bodys)])]))
