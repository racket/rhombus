#lang racket/base
(require racket/symbol
         racket/syntax-srcloc
         syntax/stx)

(provide parse-module-path-as-shrubbery)

(define (parse-module-path-as-shrubbery t)
  (syntax-case t (top)
    [(top g)
     (let ([g #'g])
       (or (syntax-case g (group parens)
             [(group s)
              (cond
                [(string? (syntax-e #'s)) #'s]
                [(identifier? #'s) #'s]
                [else #f])]
             [(group lib (parens (group s)))
              (eq? 'lib (syntax-e #'lib))
              (cond
                [(string? (syntax-e #'s))
                 (define group (stx-car g))
                 (datum->syntax group
                                (list #'lib #'s)
                                group
                                group)]
                [else #f])]
             [(group s . tail)
              (and (identifier? #'s)
                   (let loop ([tail (syntax->list #'tail)])
                     (cond
                       [(null? tail) #t]
                       [(and (pair? (cdr tail))
                             (syntax-case (car tail) (op)
                               [(op /) (eq? '/ (syntax-e #'/))]
                               [_ #f])
                             (identifier? (cadr tail)))
                        (loop (cddr tail))]
                       [else #f])))
              (datum->syntax #'s
                             (string->symbol (apply string-append
                                                    (cons (symbol->immutable-string (syntax-e #'s))
                                                          (let loop ([tail (syntax->list #'tail)])
                                                            (cond
                                                              [(null? tail) '()]
                                                              [else
                                                               (list* "/"
                                                                      (symbol->immutable-string (syntax-e (cadr tail)))
                                                                      (loop (cddr tail)))])))))
                             (let ([start (syntax-srcloc #'s)]
                                   [end (syntax-srcloc (let loop ([tail (syntax->list #'tail)])
                                                         (cond
                                                           [(null? (cdr tail)) (car tail)]
                                                           [else (loop (cdr tail))])))])
                               (if (and start
                                        end
                                        (srcloc-position start)
                                        (srcloc-position end)
                                        (srcloc-span end))
                                   (struct-copy srcloc start
                                                [span (- (+ (srcloc-position end)
                                                            (srcloc-span end))
                                                         (srcloc-position start))])
                                   #'s)))]
             [_ #f])
           (raise-syntax-error 'shrubbery "cannot convert shrubbery to an S-expression module path" g)))]
    [_
     (raise-syntax-error 'shrubbery "cannot convert multi-group shrubbery to an S-expression module path" t)]))
