#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         "binding.rkt"
         (only-in "unquote-binding-primitive.rkt"
                  pattern
                  #%parens)
         (only-in "quasiquote.rkt"
                  #%quotes
                  $)
         "parse.rkt"
         "parens.rkt")

(provide (for-space rhombus/bind
                    pattern))

;; the `pattern` form for a binding context is here;
;; the `pattern` form for `syntax_class` is in "syntax-class-clause-primitive.rkt";
;; the `pattern` form for `$` is in "unquote-binding-primitive.rkt"

(define-binding-syntax pattern
  (binding-prefix-operator
   #'pattern
   `((default . weaker))
   'macro
   (lambda (stx)
     (define (expand s tail)
       (syntax-parse #`(group #%quotes
                              (quotes (group (op $)
                                             (parens ;; needs `#%parens` binding
                                              (group . #,s)))))
         [b::binding
          (values #'b.parsed tail)]))
     (with-syntax ([pattern (syntax-parse stx
                              [(head . _)
                               (datum->syntax #'here 'pattern #'head #'head)])])
       (syntax-parse stx
         [(_ (~and a (_::alts alt ...)) . tail)
          (expand #'(pattern a)
                   #'tail)]
         [(_ (~and pat (_::quotes . _)) (~and b (_::block . _)))
          (expand #'(pattern pat b)
                  #'())]
         [(_ (~and pat (_::quotes . _)) . tail)
          (expand #'(pattern pat)
                  #'tail)])))))
