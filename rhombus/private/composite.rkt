#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         "parse.rkt"
         "binding.rkt")

(provide (for-syntax make-composite-binding-transformer))

(define-for-syntax (make-composite-binding-transformer predicate selectors)
  (lambda (tail)
    (syntax-parse tail
      [(form-id ((~datum parens) a::binding ...) . new-tail)
       #:with (a-expanded::binding-form ...) #'(a.expanded ...)
       (unless (= (length (syntax->list #'(a ...)))
                  (length selectors))
         (raise-syntax-error #f
                             (format (string-append "pattern arguments not the expected number\n"
                                                    "  expected: ~a\n"
                                                    "  given: ~a")
                                     (length selectors)
                                     (length (syntax->list #'(a ...))))
                             (syntax/loc #'form-id
                               #'(group form-id (parens a ...)))))
       (with-syntax ([falses (for/list ([a (in-list (syntax->list #'(a-expanded.var-id ... ...)))])
                               #'#f)])
         (values
          (binding-form
           #'[a-expanded.var-id ... ...]
           #`(lambda (v)
               (if (#,predicate v)
                   #,(let loop ([match? #t]
                                [a-idss (syntax->list #'((a-expanded.var-id ...) ...))]
                                [a-proc-exprs (syntax->list #'(a-expanded.check-proc-expr ...))]
                                [selectors selectors])
                       (cond
                         [(null? a-idss)
                          #`(values #,match? a-expanded.var-id ... ...)]
                         [else
                          #`(let-values ([(match? . #,(car a-idss)) (#,(car a-proc-exprs) (#,(car selectors) v))])
                              (if match?
                                  #,(loop #'match? (cdr a-idss) (cdr a-proc-exprs) (cdr selectors))
                                  (values #f . falses)))]))
                   (values #f . falses)))
           #`(begin
               a-expanded.post-defn ...))
          #'new-tail))])))
