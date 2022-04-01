#lang racket/base

(require racket/class)

(provide object?
         object-dot-lookup)

(define (object-has-field? o field)
  (and (memq field (field-names o)) #t))

(define (object-has-method? o field)
  (method-in-interface? field (object-interface o)))

(define (object-dot-lookup o field fun? args fail)
  (cond
    [(object-has-field? o field)
     (define val (dynamic-get-field field o))
     (if fun? (apply val args) val)]

    [(object-has-method? o field)
     (if fun?
         (apply dynamic-send o field args)
         (make-keyword-procedure
           (lambda (kws kwargs . pos-args)
             (keyword-apply dynamic-send kws kwargs o field pos-args))
           (lambda pos-args
             (apply dynamic-send o field pos-args))))]

    [else (fail)]))

