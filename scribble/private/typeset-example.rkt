#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre
                     shrubbery/property)
         racket/string
         racket/syntax-srcloc
         rhombus/parse
         racket/sandbox
         syntax/strip-context
         (only-in rhombus/private/srcloc
                  respan)
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
                                #:defaults ([label-expr #'(group (parsed #:rhombus/expr #f))]))
                     (~optional (group (~and no-prompt #:no_prompt))
                                #:defaults ([no-prompt #'#f]))
                     (~optional (group #:eval (block eval-expr))
                                #:defaults ([eval-expr #'(group (parsed #:rhombus/expr (make-rhombus-eval)))]))
                     (~optional (group #:hidden (block hidden-expr))
                                #:defaults ([hidden-expr #'(group (parsed #:rhombus/expr #f))]))
                     (~optional (group #:indent (block indent-expr))
                                #:defaults ([indent-expr #'(group (parsed #:rhombus/expr 0))])))
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
                                      (parens (group #:prompt (block (group (parsed #:rhombus/expr prompt))))
                                              (group #:indent (block (group (parsed #:rhombus/expr
                                                                                    (+ prompt-indent
                                                                                       (rhombus-expression indent-expr))))))
                                              (group #:inset (block (group (parsed #:rhombus/expr #f)))))
                                      (t-block t-form)))))
     (with-syntax ([((t-form e-form) ...)
                    (for/list ([form (in-list (map (lambda (s) (datum->syntax #f
                                                                              (cons 'group (syntax-e s))))
                                                   (syntax->list #'((form ...) ...))))])
                      (syntax-parse form
                        #:datum-literals (group)
                        [((~and tag group) #:blank) #'((quote #:blank) (tag (parsed #:rhombus/expr (void))))]
                        [(group #:error (block ((~and tag group) form ...)))
                         #`((list (quote #:error)
                                  #,(rb #`(#,(syntax-raw-prefix-property #'tag "") form ...)))
                            (tag form ...))]
                        [(group #:fake (block ((~and tag group) form ...)
                                              ((~and tag2 group) answer ...)))
                         #`(#,(rb #`(#,(syntax-raw-prefix-property #'tag "") form ...))
                            (tag2 answer ...))]
                        [_ #`(#,(rb form) #,form)]))])
       #'(examples
          #:eval (rhombus-expression eval-expr)
          #:label (rhombus-expression label-expr)
          #:hidden? (rhombus-expression hidden-expr)
          #:indent (rhombus-expression indent-expr)
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
                  #:indent indent
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
                                                          (apply
                                                           raise-arguments-error
                                                           'examples
                                                           "error during example"
                                                           (append
                                                            (list "example" expr)
                                                            (let ([loc (let ([l (syntax->list expr)])
                                                                         (and (pair? l)
                                                                              (eq? 'top (syntax-e (car l)))
                                                                              (pair? (cdr l))
                                                                              (null? (cddr l))
                                                                              (syntax-srcloc (respan (cadr l)))))])
                                                              (if loc
                                                                  (list "source"
                                                                        (unquoted-printing-string (srcloc->string loc)))
                                                                  null))
                                                            (list "message" msg)))))])
                           (cond
                             [(eq? mode 'error) (begin
                                                  (eval (strip-context expr))
                                                  '(oops))]
                             [else (call-with-values
                                    (lambda () (eval (strip-context expr)))
                                    list)]))])
                 (append
                  (format-lines (get-output eval) racketoutput indent)
                  (format-lines (get-error-output eval) racketerror indent)
                  (cond
                    [(eq? mode 'error)
                     (if (string? vs)
                         (format-lines vs racketerror indent)
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
                           (format-lines (get-output-string o) (lambda (s) (racketresultfont s #:decode? #f)) indent)])))]))))]))))))))
  (cond
    [hidden? null]
    [label (list label example-block)]
    [else example-block]))

(define (format-lines str format-line extra-indent)
  (for/list ([line-str (in-list (string-split str "\n"))])
    (define indent (let loop ([i 0])
                     (cond
                       [(i . >= . (string-length line-str)) i]
                       [(char=? #\space (string-ref line-str i)) (loop (+ i 1))]
                       [else i])))
    (define all-indent (+ indent extra-indent))
    (paragraph plain
               (list
                (if (= all-indent 0) "" (hspace all-indent))
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
