#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/property)
         racket/string
         rhombus/parse
         racket/sandbox
         (only-in scribble/example
                  make-base-eval
                  close-eval)
         (only-in scribble/manual
                  hspace
                  racketresultfont
                  racketerror)
         (only-in scribble/core
                  table
                  paragraph
                  nested-flow
                  style
                  plain)
         "rhombus.rhm")

(provide typeset-examples
         make-rhombus-eval
         close-eval)

(define-syntax (typeset-examples stx)
  (syntax-parse stx
    #:datum-literals (parens group block)
    [(_ (parens (~or (~optional (group #:label (block label-expr))
                                #:defaults ([label-expr #'(group (parsed "Examples:"))]))
                     (~optional (group #:eval (block eval-expr))
                                #:defaults ([eval-expr #'(group (parsed (make-rhombus-eval)))]))
                     (~optional (group #:hidden (block hidden-expr))
                                #:defaults ([hidden-expr #'(group (parsed #f))])))
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
                    (for/list ([form (in-list (syntax->list #'((group form ...) ...)))])
                      (syntax-parse form
                        #:datum-literals (group)
                        [(group #:blank) #'(quote #:blank)]
                        [(group #:error form ...) #`(list (quote #:error)
                                                          #,(rb #'(group form ...)))]
                        [_ (rb form)]))])
       #'(examples
          #:eval (rhombus-expression eval-expr)
          #:label (rhombus-expression label-expr)
          #:hidden? (rhombus-expression hidden-expr)
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
                  #:hidden? hidden?
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
           (define rb (car rb+expr))
           (cond
             [(eq? rb '#:blank)
              (list (paragraph plain (hspace 1)))]
             [else
              (define-values (plain-rb mode)
                (cond
                  [(and (pair? rb) (eq? (car rb) '#:error))
                   (values (cadr rb) 'error)]
                  [else (values rb 'success)]))
              (define expr (cadr rb+expr))
              (cons
               plain-rb
               (let ([v (with-handlers ([exn:fail? (lambda (exn)
                                                     (define msg (format-exception exn eval))
                                                     (if (eq? mode 'error)
                                                         msg
                                                         (raise-arguments-error
                                                          'examples
                                                          "error during example"
                                                          "example" expr
                                                          "message" msg)))])
                          (cond
                            [(eq? mode 'error) (begin
                                                 (eval expr)
                                                 'oops)]
                            [else (eval expr)]))])
                 (cond
                   [(eq? mode 'error)
                    (if (string? v)
                        (format-lines v racketerror)
                        (raise-arguments-error
                         'examples
                         "example did not error"
                         "example" expr))]
                   [(void? v) null]
                   [else
                    (define o (open-output-string))
                    (call-in-sandbox-context eval (lambda () (print v o)))
                    (format-lines (get-output-string o) racketresultfont)])))]))))))))
  (cond
    [hidden? null]
    [label (list label example-block)]
    [else example-block]))

(define (format-lines str format-line)
  (for/list ([line-str (in-list (string-split str "\n"))])
    (paragraph plain (racketresultfont line-str))))

(define (format-exception exn eval)
  (define o (open-output-string))
  (call-in-sandbox-context eval (lambda ()
                                  (parameterize ([error-print-context-length 0]
                                                 [current-error-port o])
                                    ((error-display-handler) (exn-message exn) exn))))
  (get-output-string o))
