#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     shrubbery/property)
         racket/string
         rhombus/parse
         racket/sandbox
         syntax/strip-context
         (only-in scribble/example
                  make-base-eval
                  close-eval)
         (only-in scribble/manual
                  hspace
                  racketresultfont
                  racketoutput
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
                                #:defaults ([label-expr #'(group (parsed #f))]))
                     (~optional (group (~and no-prompt #:no_prompt))
                                #:defaults ([no-prompt #'#f]))
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
                                                [(_ (a . _) . _) #'a]
                                                [(_ a . _) #'a]))
                               "")]
                     [prompt (if (syntax-e #'no-prompt) "" "> ")]
                     [prompt-indent (if (syntax-e #'no-prompt) 0 2)])
         #'(rhombus-expression (group rhombusblock_etc
                                      (parens (group #:prompt (block (group (parsed prompt))))
                                              (group #:indent (block (group (parsed prompt-indent))))
                                              (group #:inset (block (group (parsed #f)))))
                                      (t-block t-form)))))
     (with-syntax ([((t-form e-form) ...)
                    (for/list ([form (in-list (syntax->list #'((group form ...) ...)))])
                      (syntax-parse form
                        #:datum-literals (group)
                        [((~and tag group) #:blank) #'((quote #:blank) (tag (parsed (void))))]
                        [(group #:error (block ((~and tag group) form ...)))
                         #`((list (quote #:error)
                                  #,(rb #`(#,(syntax-raw-prefix-property #'tag "") form ...)))
                            (tag form ...))]
                        [_ #`(#,(rb form) #,form)]))])
       #'(examples
          #:eval (rhombus-expression eval-expr)
          #:label (rhombus-expression label-expr)
          #:hidden? (rhombus-expression hidden-expr)
          (list
           (list t-form
                 (adjust-top-srcloc (quote-syntax (top e-form))))
           ...)))]))

(define (make-rhombus-eval)
  (define eval (make-base-eval #:lang 'rhombus
                               '(top)))
  (call-in-sandbox-context eval (lambda () (dynamic-require '(submod rhombus configure-runtime) #f)))
  (call-in-sandbox-context eval (lambda () (error-print-source-location #f)))
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
               (let ([vs (with-handlers ([exn:fail? (lambda (exn)
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
                                                  (eval (strip-context expr))
                                                  '(oops))]
                             [else (call-with-values
                                    (lambda () (eval (strip-context expr)))
                                    list)]))])
                 (append
                  (format-lines (get-output eval) racketoutput)
                  (cond
                    [(eq? mode 'error)
                     (if (string? vs)
                         (format-lines vs racketerror)
                         (raise-arguments-error
                          'examples
                          "example did not error"
                          "example" expr))]
                    [else
                     (apply
                      append
                      (for/list ([v (in-list vs)])
                        (cond
                          [(void? v) null]
                          [else
                           (define o (open-output-string))
                           (call-in-sandbox-context eval (lambda () (print v o)))
                           (format-lines (get-output-string o) (lambda (s) (racketresultfont s #:decode? #f)))])))]))))]))))))))
  (cond
    [hidden? null]
    [label (list label example-block)]
    [else example-block]))

(define (format-lines str format-line)
  (for/list ([line-str (in-list (string-split str "\n"))])
    (define indent (let loop ([i 0])
                     (cond
                       [(i . >= . (string-length line-str)) i]
                       [(char=? #\space (string-ref line-str i)) (loop (+ i 1))]
                       [else i])))
    (paragraph plain
               (list
                 (if (= indent 0) "" (hspace indent))
                 (format-line (substring line-str indent))))))

(define (format-exception exn eval)
  (define o (open-output-string))
  (call-in-sandbox-context eval (lambda ()
                                  (parameterize ([error-print-context-length 0]
                                                 [current-error-port o])
                                    ((error-display-handler) (exn-message exn) exn))))
  (get-output-string o))

(define (adjust-top-srcloc stx)
  (define l (syntax->list stx))
  (datum->syntax #f
                 (cons (syntax-property (car l) 'raw "")
                       (cdr l))
                 (cadr l)))

  
  


