#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "parse.rkt"
         (submod "function-parse.rkt" for-call)
         (submod "indexable.rkt" for-ref)
         (submod "list.rkt" for-binding)
         (submod "list.rkt" for-implicit)
         "setmap.rkt"
         "literal.rkt"
         "parens.rkt"
         (submod "static-info.rkt" for-literal)
         "is-static.rkt")

(provide (for-space #f
                    #%body
                    #%block
                    #%index
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
                    #%index
                    #%literal
                    #%parens
                    #%brackets
                    #%braces
                    #%call))

(define-syntax #%body
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (tag::block . body))
        (values (datum->syntax #f (cons (datum->syntax #'here 'rhombus-body #'tag #'tag) #'body) #'tag)
                #'())]))))

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
        (values (relocate #'datum #'(quote datum) #'datum) ; copies all props, including originalness
                #'tail)]))))

(define-binding-syntax #%literal
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ datum . tail)
        (when (keyword? (syntax-e #'datum)) (raise-keyword-error #'datum))
        (values (binding-form #'literal-infoer
                              #'(datum))
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
                                      (or (quoted-static-infos #'datum)
                                          #'())
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
       [(_ (~and head (_::parens . args)) . tail)
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
               [e::expression (values (relocate+reraw (maybe-respan #'head) #'e.parsed) #'tail)])]))]))))

(define-binding-syntax #%parens
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(_ (~and head (_::parens . args)) . tail)
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
       [(_ (~and head (_::parens . args)) . tail)
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty repetition" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many repetitions" #'head)]
            [else
             (syntax-parse (car args)
               [r::repetition (values #'r.parsed #'tail)])]))]))))

(define-syntax #%call
  (expression-infix-operator
   (expr-quote #%call)
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (define static? (is-static-context/tail? stxes))
     (parse-function-call rator '() stxes #:static? static?))
   'left))

(define-repetition-syntax #%call
  (repetition-infix-operator
   (repet-quote #%call)
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (define static? (is-static-context/tail? stxes))
     (parse-function-call rator '() stxes #:static? static? #:repetition? #t))
   'left))

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

(define-syntax #%index
  (expression-infix-operator
   (expr-quote #%index)
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (define more-static? (is-static-context/tail? stxes))
     (parse-indexable-ref-or-set array stxes more-static?))
   'left))

(define-repetition-syntax #%index
  (repetition-infix-operator
   (repet-quote #%index)
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (define more-static? (is-static-context/tail? stxes))
     (parse-indexable-ref-or-set array stxes more-static? #:repetition? #t))
   'left))

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
