#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "tail.rkt")
         "name-root.rkt"
         (for-syntax "name-root.rkt"))

(provide static_info
         (for-syntax static_info_ct))

(module+ for-static-info
  (provide (for-syntax (rename-out [pack pack-static-info]))))

(define-syntax static_info
  (simple-name-root))

(begin-for-syntax
  (define-syntax static_info_ct
    (simple-name-root pack)))

(define-for-syntax (pack v)
  (datum->syntax
   #f
   (map (lambda (p)
          (syntax-parse p
            #:datum-literals (group parens)
            [(parens (group key) (group val))
             #'(key val)]))
        (syntax->list (unpack-tail v pack)))))
