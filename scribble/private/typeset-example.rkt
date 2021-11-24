#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/property)
         racket/string
         rhombus/parse
         racket/sandbox
         (only-in scribble/example
                  make-base-eval)
         (only-in scribble/manual
                  hspace
                  racketresultfont)
         (only-in scribble/core
                  table
                  paragraph
                  nested-flow
                  style
                  plain)
         "rhombus.rhm")

(provide typeset-examples)

(define-syntax (typeset-examples stx)
  (syntax-parse stx
    #:datum-literals (parens group block)
    [(_ (parens (~or (~optional (group #:label (block label-expr))
                                #:defaults ([label-expr #'(group (parsed "Examples:"))])))
                ...
                (group form ...) ...))
     (define (rb form)
       (with-syntax ([t-form form]
                     [t-block (syntax-raw-property
                               (datum->syntax #f 'block
                                              (syntax-parse form
                                                [(_ a . _) #'a]))
                               "")])
         #'(rhombus-expression (group rhombusblock
                                      (parens (group #:prompt (block (group (parsed ">"))))
                                              (group #:indent (block (group (parsed 2))))
                                              (group #:inset (block (group (parsed #f)))))
                                      (t-block t-form)))))
     (with-syntax ([(t-form ...)
                    (map rb (syntax->list #'((group form ...) ...)))])
       #'(examples
          #:eval (make-rhombus-eval)
          #:label (rhombus-expression label-expr)
          (list
           (list t-form
                 '(top (group form ...)))
           ...)))]))

(define (make-rhombus-eval)
  (define eval (make-base-eval #:lang 'rhombus
                               '(top)))
  (call-in-sandbox-context eval (lambda () (dynamic-require '(submod rhombus configure-runtime) #f)))
  eval)

(define (examples #:eval eval
                  #:label label
                  rb+exprs)
  (define example-block
    (nested-flow
     (style 'code-inset null)
     (list
      (table
       plain
       (map
        list
        (apply
         append
         (for/list ([rb+expr (in-list rb+exprs)])
           (cons
            (car rb+expr)
            (let ([v (eval (cadr rb+expr))])
              (cond
                [(void? v) null]
                [else
                 (define o (open-output-string))
                 (call-in-sandbox-context eval (lambda () (print v o)))
                 (for/list ([str (in-list (string-split (get-output-string o) "\n"))])
                   (paragraph plain (racketresultfont str)))]))))))))))
  (if label
      (list label example-block)
      example-block))

