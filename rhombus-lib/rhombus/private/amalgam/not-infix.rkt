#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     enforest/name-parse
                     enforest/hier-name-parse
                     enforest/syntax-local
                     enforest/operator
                     "name-path-op.rkt")
         "expression.rkt"
         "repetition.rkt"
         "dotted-sequence-parse.rkt"
         "name-root-space.rkt"
         "name-root-ref.rkt"
         "parse.rkt"
         "membership-testable.rkt"
         (only-in "arithmetic.rkt" is_now)
         (only-in "annotation.rkt" is_a)
         (only-in "match.rkt" matches)
         (submod "arithmetic.rkt" parse-not))

(define-for-syntax (parse-not form1 tail repet?)
  (syntax-parse tail
    [(self::name seq::dotted-operator-or-identifier-sequence . tail)
     (define in-space (if repet? in-repetition-space in-expression-space))
     (syntax-parse #'seq
       [(~var op (:hier-name-seq in-name-root-space in-space name-path-op name-root-ref))
        (define mode
          (cond
            [(free-identifier=? #'op.name #'in) 'in]
            [(free-identifier=? #'op.name #'is_now) 'is_now]
            [(free-identifier=? #'op.name #'is_a) 'is_a]
            [(free-identifier=? #'op.name #'matches) 'matches]
            [else #f]))
        (unless mode
          (raise-syntax-error #f "expected a negatable operator" #'self #'op))
        (define infix-op (syntax-local-value* (in-space #'op.name) (if repet?
                                                                       repetition-infix-operator-ref
                                                                       expression-infix-operator-ref)))
        (define (apply-op form1 form2)
          ((operator-proc infix-op) form1 form2 #'op.name 'invert))
        (case mode
          [(in is_now)
           (cond
             [(not repet?)
              (syntax-parse #'(group . tail)
                [(~var form2 (:infix-op+expression+tail #'self.name))
                 (values (apply-op form1 #'form2.parsed)
                         #'form2.tail)])]
             [else
              (syntax-parse #'(group . tail)
                [(~var form2 (:infix-op+repetition-use+tail #'self.name))
                 (values (apply-op form1 #'form2.parsed)
                         #'form2.tail)])])]
          [(is_a matches)
           ((operator-proc infix-op) form1 (cons #'op.name #'tail) 'invert)])])]
    [(self next . _)
     (raise-syntax-error #f "expected negatable operator" #'self #'next)]))

(begin-for-syntax
  (set-parse-not! parse-not))
