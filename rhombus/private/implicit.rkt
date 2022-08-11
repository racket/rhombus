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
         "setmap.rkt"
         (submod "map.rkt" for-binding)
         "literal.rkt"
         "parens.rkt")

(provide #%body
         #%literal
         #%tuple
         ;; #%quote is provided by "quasiquote.rkt"
         #%call
         #%array
         #%ref
         #%set)

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
  (make-expression+binding-prefix-operator
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
                #'tail)]))))

(define-for-syntax (raise-keyword-error datum)
  (raise-syntax-error #f
                      "misplaced keyword"
                      datum))

(define-syntax #%tuple
  (make-expression+binding+repetition-prefix-operator
   #'#%tuple
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
  (expression-infix-operator
   #'#%call
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator stxes))
   'left))

(define-syntax #%array
  (make-expression+binding+repetition-prefix-operator
   #'#%array
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

(define-syntax #%ref
  (expression-infix-operator
   #'#%ref
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes))
   'left))

(define-syntax #%set
  (make-expression+binding-prefix-operator
   #'#%set
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
     (parse-map-binding 'braces stxes "braces"))))
