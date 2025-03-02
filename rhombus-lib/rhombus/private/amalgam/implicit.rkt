#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/print
                     "srcloc.rkt")
         "expression.rkt"
         "binding.rkt"
         "repetition.rkt"
         "entry-point.rkt"
         "immediate-callee.rkt"
         "parse.rkt"
         (submod "function-parse.rkt" for-call)
         (submod "indexable.rkt" for-ref)
         (submod "list.rkt" for-binding)
         (submod "list.rkt" normal-call)
         "setmap.rkt"
         "literal.rkt"
         "parens.rkt"
         "static-info.rkt"
         (submod "literal.rkt" for-info)
         "is-static.rkt"
         "ends-parse.rkt"
         (only-in "op-literal.rkt"
                  :_-expr)
         "arrow-annotation.rkt")

(provide (for-space #f
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
                    #%call)
         (for-space rhombus/entry_point
                    #%parens)
         (for-space rhombus/immediate_callee
                    #%parens))

(module+ normal-call
  (provide (for-syntax normal-call?)))

(module+ normal-literal
  (provide (for-syntax normal-literal?)))

(define-syntax #%block
  (expression-prefix+infix-operator
   (expression-transformer
    (lambda (stxes)
      (syntax-parse stxes
        [(_ b)
         (raise-syntax-error #f
                             "misplaced;\n not allowed as an expression by itself"
                             #'b)])))
   (expression-infix-operator
    #f
    '((default . stronger))
    'macro
    (lambda (form stxes)
      (syntax-parse stxes
        [(_ b)
         (raise-syntax-error #f
                             "misplaced;\n not allowed after an expression as an expression by itself"
                             #'b)]))
    'left)))

(define-binding-syntax #%block
  (binding-prefix+infix-operator
   (binding-transformer
    (lambda (stxes)
      (syntax-parse stxes
        [(_ b)
         (raise-syntax-error #f
                             "misplaced;\n not allowed as a binding by itself"
                             #'b)])))
   (expression-infix-operator
    #f
    '((default . stronger))
    'macro
    (lambda (form stxes)
      (syntax-parse stxes
        [(_ b)
         (raise-syntax-error #f
                             "misplaced;\n not allowed after a binding as a binding by itself"
                             #'b)]))
    'left)))

(define-syntax #%literal
  (expression-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(form-id datum . tail)
        (check-literal-term #'form-id #'datum)
        (define quoted-datum
          ;; but reraw here
          (reraw (list #'form-id #'datum)
                 ;; copies all props, including originalness
                 (relocate #'datum
                           #`(quote #,(no-srcloc #'datum))
                           #'datum)))
        (values (cond
                  [(literal-static-infos #'datum)
                   => (lambda (si)
                        (wrap-static-info* quoted-datum si))]
                  [else quoted-datum])
                #'tail)]))))

(define-binding-syntax #%literal
  (binding-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(form-id datum . tail)
        (check-literal-term #'form-id #'datum)
        (values (binding-form #'literal-infoer
                              #`([datum #,(shrubbery-syntax->string #'datum)]))
                #'tail)]))))

(define-repetition-syntax #%literal
  (repetition-transformer
   (lambda (stxes)
     (syntax-parse stxes
       [(form-id datum . tail)
        (check-literal-term #'form-id #'datum)
        (values (make-repetition-info #'datum
                                      '()
                                      #'(quote datum)
                                      (or (literal-static-infos #'datum)
                                          #'())
                                      0)
                #'tail)]))))

(define-for-syntax (check-literal-term form-id d-stx)
  (define d (syntax-e d-stx))
  (when (or (symbol? d)
            (keyword? d)
            (pair? d)
            (null? d))
    (raise-syntax-error #f
                        "not an allowed literal term"
                        (respan (datum->syntax #f (list form-id d-stx)))
                        d-stx)))

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
             (syntax-parse (car args)
               ;; check for anonymous-function shorthand:
               [(_ ... _::_-expr . _) (values (build-anonymous-function (car args) #'head) #'tail)]
               ;; eagerly parse content of parentheses; we could choose to
               ;; delay parsing by using `rhombus-expression`, instead
               [e::expression (values (relocate+reraw (maybe-respan #'head) #'e.parsed) #'tail)])]))]))))

(define-entry-point-syntax #%parens
  (entry-point-transformer
   ;; parse function:
   (lambda (stx adjustments)
     (syntax-parse stx
       [(_ (~and head (_::parens . args)))
        (let ([args (syntax->list #'args)])
          (cond
            [(null? args)
             (raise-syntax-error #f "empty entry point" #'head)]
            [(pair? (cdr args))
             (raise-syntax-error #f "too many expressions" #'head)]
            [else
             (syntax-parse (car args)
               ;; check for anonymous-function shorthand:
               [(_ ... _::_-expr . _) (build-anonymous-function (car args) #'head
                                                                #:adjustments adjustments)]
               [(~var e (:entry-point adjustments)) #'e.parsed]
               [_ (raise-syntax-error #f "not an entry point" #'head)])]))]))
   ;; extract shape:
   (lambda (stx)
     (syntax-parse stx
       #:datum-literals (group)
       [(_ (~and head (_::parens arg-g)))
        (syntax-parse #'arg-g
          [(_ ... _::_-expr . _)
           (hash
            'arity
            (arithmetic-shift
             1
             (for/sum ([t (in-list (cdr (syntax->list #'arg-g)))])
               (syntax-parse t
                 [_::_-expr 1]
                 [_ 0]))))]
          [e::entry-point-shape (syntax->datum #'e.parsed)]
          [_ #f])]
       [_ #f]))))

(define-immediate-callee-syntax #%parens
  (immediate-callee-transformer
   ;; parse function:
   (lambda (stx static-infoss op-mode op-stx)
     (syntax-parse stx
       [(_ (~and head (_::parens arg)) . tail)
        #:when (do-ends-parse? op-mode op-stx #'tail
                               in-expression-space expression-relative-precedence expression-infix-operator-ref)
        (syntax-parse #'arg
          ;; check for anonymous-function shorthand:
          [(_ ... _::_-expr . _)
           (pack-immediate-callee (build-anonymous-function #'arg #'head
                                                            #:argument-static-infoss static-infoss)
                                  #'tail)]
          [(~var e (:immediate-callee static-infoss 'prefix #'none))
           (define-values (parsed new-tail) (unpack-immediate-callee #'e.parsed))
           (pack-immediate-callee parsed #'tail)]
          [e::expression
           (pack-immediate-callee #'e.parsed #'tail)])]
       [_
        (case op-mode
          [(prefix)
           (syntax-parse #`(group . #,stx)
             [(~var e (:prefix-op+expression+tail op-stx))
              (pack-immediate-callee #'e.parsed #'e.tail)])]
          [else
           (syntax-parse #`(group . #,stx)
             [(~var e (:infix-op+expression+tail op-stx))
              (pack-immediate-callee #'e.parsed #'e.tail)])])]))))

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
   #f
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (define static? (is-static-context/tail? stxes))
     (define-values (proc tail to-anon-function?)
       (parse-function-call rator '() stxes
                            #:static? static?
                            #:can-anon-function? #t))
     (values proc tail))
   'left))

(define-repetition-syntax #%call
  (repetition-infix-operator
   #f
   '((default . stronger))
   'macro
   (lambda (rator stxes)
     (define static? (is-static-context/tail? stxes))
     (define-values (proc tail to-anon-function?)
       (parse-function-call rator '() stxes
                            #:static? static?
                            #:repetition? #t))
     (values proc tail))
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
   #f
   '((default . stronger))
   'macro
   (lambda (array stxes)
     (define more-static? (is-static-context/tail? stxes))
     (parse-indexable-ref-or-set array stxes more-static?))
   'left))

(define-repetition-syntax #%index
  (repetition-infix-operator
   #f
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

(define-for-syntax (normal-call? tag)
  (free-identifier=? (datum->syntax tag '#%call)
                     (expr-quote #%call)))

(define-for-syntax (normal-literal? lit)
  (free-identifier=? (datum->syntax lit '#%literal)
                     (expr-quote #%literal)))

(begin-for-syntax
  (install-normal-call?! normal-call?))
