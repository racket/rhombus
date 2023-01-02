#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "expression+binding.rkt"
         "repetition.rkt"
         "parse.rkt"
         (submod "function.rkt" for-call)
         (submod "map-ref.rkt" for-ref)
         (submod "list.rkt" for-binding)
         (submod "list.rkt" for-implicit)
         "setmap.rkt"
         "literal.rkt"
         "parens.rkt")

(provide #%body
         #%literal
         #%parens
         ;; `#%quotes` provided by "quasiquote.rkt"
         #%call
         #%brackets
         #%ref
         #%braces)

(module+ for-dynamic-static
  (provide (for-syntax make-#%ref)))

(define-syntax #%body
  (expression-prefix-operator
   #'#%body
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~and tag (~datum block)) . body)) . tail)
        (values (datum->syntax #f (cons (datum->syntax #'here 'rhombus-body #'tag #'tag) #'body) #'tag)
                #'tail)]))))

(define-syntax #%literal
  (make-expression+binding+repetition-prefix-operator
   #'#%literal
   '((default . stronger))
   'macro
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (syntax/loc #'datum (quote datum))
                #'tail)]))
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (binding-form #'literal-infoer
                              #'datum)
                #'tail)]))
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (make-repetition-info #'datum
                                      #'value
                                      (syntax/loc #'datum (quote datum))
                                      0
                                      0
                                      #'()
                                      #t)
                #'tail)]))))

(define-for-syntax (raise-keyword-error datum)
  (raise-syntax-error #f
                      "misplaced keyword"
                      datum))

(define-syntax #%parens
  (make-expression+binding+repetition-prefix-operator
   #'#%parens
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty expression" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many expressions" #'head)]
            [else
             ;; eagerly parse content of parentheses; we could choose to
             ;; delay parsing by using `rhombus-expression`, instead
             (syntax-parse (car args)
               [e::expression (values #'e.parsed #'tail)])]))]))
   ;; binding
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty pattern" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many patterns" #'head)]
            [else
             (syntax-parse (car args)
               [b::binding (values #'b.parsed #'tail)])]))]))
   ;; repetition
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty repetition" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many repetions" #'head)]
            [else
             (syntax-parse (car args)
               [r::repetition (values #'r.parsed #'tail)])]))]))))

(define-syntax #%call
  (make-expression+repetition-infix-operator
   #'#%call
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator '() stxes))
   (lambda (rator stxes)
     (parse-function-call rator '() stxes #:repetition? #t))
   'left))

(define-syntax #%brackets
  (make-expression+binding+repetition-prefix-operator
   #'#%brackets
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-expression stxes))
   ;; binding
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-binding stxes))
   ;; repetition
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-repetition stxes))))

(define-for-syntax (check-brackets stxes)
  (syntax-parse stxes
    [(_ (_::brackets . _) . _) (void)]))

(define-for-syntax (make-#%ref more-static?)
  (make-expression+repetition-infix-operator
   #'#%ref
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static?))
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static? #:repetition? #t))
   'left))

(define-syntax #%ref
  (make-#%ref #f))

(define-syntax #%braces
  (make-expression+binding+repetition-prefix-operator
   #'#%braces
   '((default . stronger))
   'macro
   ;; expression
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces)
                #'tail)]))
   ;; binding
   (lambda (stxes)
     (parse-setmap-binding 'braces stxes))
   ;; repetition
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces #:repetition? #t)
                #'tail)]))))

(begin-for-syntax
  (set-#%call-id! (quote-syntax #%call)))
