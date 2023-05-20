#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "provide.rkt"
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "parse.rkt"
         (submod "function-parse.rkt" for-call)
         (submod "map-ref.rkt" for-ref)
         (submod "list.rkt" for-binding)
         (submod "list.rkt" for-implicit)
         "setmap.rkt"
         "literal.rkt"
         "parens.rkt")

(provide (for-space #f
                    #%body
                    #%block
                    #%ref
                    #%literal
                    ;; `#%quotes` provided by "quasiquote.rkt"
                    #%parens
                    #%brackets
                    #%braces
                    #%call)
         (for-space rhombus/bind
                    #%block
                    #%literal
                    #%parens
                    #%brackets
                    #%braces)
         (for-space rhombus/repet
                    #%ref
                    #%literal
                    #%parens
                    #%brackets
                    #%braces
                    #%call))

(module+ for-dynamic-static
  (provide (for-spaces (#f
                        rhombus/repet)
                       #%ref
                       static-#%ref
                       #%call
                       static-#%call)))

(define-syntax #%body
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~and tag (~datum block)) . body)) . tail)
        (values (datum->syntax #f (cons (datum->syntax #'here 'rhombus-body #'tag #'tag) #'body) #'tag)
                #'tail)]))))

(define-syntax #%block
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ b)
        (raise-syntax-error #f
                            "misplaced;\n not allowed as an expression by itself"
                            #'b)]))))

(define-binding-syntax #%block
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ b)
        (raise-syntax-error #f
                            "misplaced;\n not allowed as a binding by itself"
                            #'b)]))))

(define-syntax #%literal
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (syntax/loc #'datum (quote datum))
                #'tail)]))))

(define-binding-syntax #%literal
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (binding-form #'literal-infoer
                              #'datum)
                #'tail)]))))

(define-repetition-syntax #%literal
  (repetition-transformer
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
  (expression-transformer
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
               [e::expression (values #'e.parsed #'tail)])]))]))))

(define-binding-syntax #%parens
  (binding-transformer
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
               [b::binding (values #'b.parsed #'tail)])]))]))))

(define-repetition-syntax #%parens
  (repetition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head ((~datum parens) . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty repetition" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many repetitions" #'head)]
            [else
             (syntax-parse (car args)
               [r::repetition (values #'r.parsed #'tail)])]))]))))

(define-for-syntax (make-#%call-expression name static?)
  (expression-infix-operator
   name
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator '() stxes #:static? static?))
   'left))

(define-syntax #%call (make-#%call-expression (expr-quote #%call) #f))
(define-syntax static-#%call (make-#%call-expression (expr-quote static-#%call) #t))

(define-for-syntax (make-#%call-repetition name static?)
  (repetition-infix-operator
   name
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (parse-function-call rator '() stxes #:static? static? #:repetition? #t))
   'left))

(define-repetition-syntax #%call (make-#%call-repetition (repet-quote #%call) #f))
(define-repetition-syntax static-#%call (make-#%call-repetition (repet-quote static-#%call) #t))

(define-syntax #%brackets
  (expression-transformer
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-expression stxes))))

(define-binding-syntax #%brackets
  (binding-transformer
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-binding stxes))))

(define-repetition-syntax #%brackets
  (repetition-transformer
   (lambda (stxes)
     (check-brackets stxes)
     (parse-list-repetition stxes))))

(define-for-syntax (check-brackets stxes)
  (syntax-parse stxes
    [(_ (_::brackets . _) . _) (void)]))

(define-for-syntax (make-#%ref name more-static?)
  (expression-infix-operator
   name
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static?))
   'left))

(define-syntax #%ref
  (make-#%ref (expr-quote #%ref) #f))
(define-syntax static-#%ref
  (make-#%ref (expr-quote #%static-ref) #t))

(define-for-syntax (make-repetition-#%ref name more-static?)
  (repetition-infix-operator
   name
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (parse-map-ref-or-set array stxes more-static? #:repetition? #t))
   'left))

(define-repetition-syntax #%ref
  (make-repetition-#%ref (repet-quote #%ref) #f))
(define-repetition-syntax static-#%ref
  (make-repetition-#%ref (repet-quote static-#%ref) #t))

(define-syntax #%braces
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces)
                #'tail)]))))

(define-binding-syntax #%braces
  (binding-transformer
   (lambda (stxes)
     (parse-setmap-binding 'braces stxes))))

(define-repetition-syntax #%braces
  (repetition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ braces . tail)
        (values (parse-setmap-expression #'braces #:repetition? #t)
                #'tail)]))))

(begin-for-syntax
  (set-#%call-ids! (quote-syntax #%call)
                   (quote-syntax static-#%call)))
