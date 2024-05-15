#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/string
         racket/syntax-srcloc
         rhombus/parse
         racket/sandbox
         file/convertible
         racket/port
         syntax/strip-context
         (only-in rhombus/private/srcloc
                  respan)
         (only-in scribble/example
                  make-base-eval
                  make-base-eval-factory
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
         "rhombus.rhm"
         "hspace.rkt"
         rhombus/private/version-case
         ;; so a sandbox can attach these:
         (only-in rhombus)
         (only-in rhombus/meta)
         (only-in rhombus/private/runtime-config))

(provide typeset-examples
         make-rhombus-eval
         close-eval)

(define-syntax (typeset-examples stx)
  (syntax-parse stx
    #:datum-literals (parens group block)
    [(_ (parens (~alt (~optional (group #:label (block label-expr))
                                 #:defaults ([label-expr #'(group (parsed #:rhombus/expr #f))]))
                      (~optional (group (~and #:no_prompt (~bind [no-prompt? #t])))
                                 #:defaults ([no-prompt? #f]))
                      (~optional (group (~and eval-kw #:eval) (block eval-expr))
                                 #:defaults ([eval-expr #'(group (parsed #:rhombus/expr (make-rhombus-eval)))]))
                      (~optional (group (~and once-kw #:once)))
                      (~optional (group #:hidden (block hidden-expr))
                                 #:defaults ([hidden-expr #'(group (parsed #:rhombus/expr #f))]))
                      (~optional (group #:result_only (block result-only-expr))
                                 #:defaults ([result-only-expr #'(group (parsed #:rhombus/expr #f))]))
                      (~optional (group #:indent (block indent-expr))
                                 #:defaults ([indent-expr #'(group (parsed #:rhombus/expr 0))])))
                ...
                (~and g (group _ ...)) ...))
     (define (rb form)
       (define-values (block-stx spliced?)
         (syntax-parse form
           #:datum-literals (group block)
           [(group (~and b (block . _))) (values #'b #t)]
           [(group t ...) (values #'(block (group t ...)) #f)]))
       #`(rhombus-expression
          (group rhombusblock_etc
                 (parens (group #:prompt (block (group (parsed #:rhombus/expr
                                                               '#,(if (attribute no-prompt?) "" "> ")))))
                         (group #:indent (block (group (parsed #:rhombus/expr
                                                               (+ '#,(if (attribute no-prompt?) 0 2)
                                                                  (rhombus-expression indent-expr))))))
                         (group #:inset (block (group (parsed #:rhombus/expr #f))))
                         (group #:indent_from_block (block (group (parsed #:rhombus/expr '#,spliced?)))))
                 #,block-stx)))
     (with-syntax ([((t-form e-form) ...)
                    (for/list ([form (in-list (syntax->list #'(g ...)))])
                      (syntax-parse form
                        #:datum-literals (group block)
                        [(group #:blank)
                         (list #'(quote #:blank)
                               #'(group (parsed #:rhombus/expr (void))))]
                        [(group #:error (block (~and t-form e-form)))
                         (list #`(list (quote #:error) #,(rb #'t-form)) #'e-form)]
                        [(group #:check (block (~and t-form e-form1)
                                               (group #:is expect ...)))
                         (list #`(list (quote #:check) #,(rb #'t-form))
                               #'(e-form1 (group expect ...)))]
                        [(group #:fake (block t-form e-form))
                         (list (rb #'t-form) #'e-form)]
                        [_ (list (rb form) form)]))])
       #`(examples
          #:eval (rhombus-expression eval-expr)
          #:once? #,(and (or (attribute once-kw) (not (attribute eval-kw))) #t)
          #:label (rhombus-expression label-expr)
          #:hidden? (rhombus-expression hidden-expr)
          #:result-only? (rhombus-expression result-only-expr)
          #:indent (rhombus-expression indent-expr)
          (list
           (list t-form
                 (adjust-top-srcloc (quote-syntax (top e-form))))
           ...)))]))

(define (make-rhombus-eval [lang 'rhombus]
                           #:attach [attach? #t])
  ;; `make-base-eval` attaches `file/convertible`
  (define eval
    (parameterize ([sandbox-namespace-specs
                    (append (sandbox-namespace-specs)
                            (if attach?
                                (meta-if-version-at-least
                                 "8.13.0.4"
                                 '(rhombus
                                   rhombus/meta
                                   rhombus/private/runtime-config)
                                 '())
                                '()))])
      (make-base-eval #:lang lang
                      '(top))))
  (call-in-sandbox-context eval (lambda () (dynamic-require '(submod rhombus configure-runtime) #f)))
  (call-in-sandbox-context eval (lambda () (error-print-source-location #f)))
  eval)

(define (examples #:eval eval
                  #:once? once?
                  #:label label
                  #:hidden? hidden?
                  #:result-only? result-only?
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
              (if result-only?
                  null
                  (list (paragraph plain (hspace 1))))]
             [else
              (define-values (plain-rb mode)
                (cond
                  [(and (pair? rb) (eq? (car rb) '#:error))
                   (values (cadr rb) 'error)]
                  [(and (pair? rb) (eq? (car rb) '#:check))
                   (values (cadr rb) 'check)]
                  [else (values rb 'success)]))
              (define expr (cadr rb+expr))
              ((if result-only?
                   (lambda (a b) b)
                   cons)
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
                             [(eq? mode 'check)
                              (define exprs (syntax->list (cadr (syntax->list expr))))
                              (define vals
                                (call-with-values
                                 (lambda () (eval (strip-context #`(top #,(car exprs)))))
                                 list))
                              (define expects
                                (call-with-discarded-output
                                 eval
                                 (lambda ()
                                   (call-with-values
                                    (lambda () (eval (strip-context #`(top #,(cadr exprs)))))
                                    list))))
                              (unless (equal-always? vals expects)
                                (error "check failed"))
                              vals]
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
                           (define-values (in out) (make-pipe-with-specials))
                           ;; set the print handler so that convertibles are kept intact:
                           (let ([orig-print (port-print-handler out)])
                             (port-print-handler out (lambda (v o [mode 0])
                                                       (if (convertible? v)
                                                           (write-special v o)
                                                           (orig-print v o mode)))))
                           (call-in-sandbox-context eval (lambda () (print v out)))
                           (close-output-port out)
                           (format-lines in
                                         (lambda (s) (racketresultfont (keep-spaces s) #:decode? #f))
                                         indent)])))]))))]))))))))
  (when once? (close-eval eval))
  (cond
    [hidden? null]
    [label (list label example-block)]
    [else example-block]))

(define (format-lines in-or-str format-str extra-indent)
  (define split-input
    (if (string? in-or-str)
        (map list (string-split in-or-str "\n"))
        (let loop ([accum null])
          (define v (read-char-or-special in-or-str))
          (cond
            [(eof-object? v) (list (list (list->string (reverse accum))))]
            [(eqv? v #\newline)
             (cons (list (list->string (reverse accum)))
                   (loop null))]
            [(char? v) (loop (cons v accum))]
            [else
             (define l (loop null))
             (cons (list* (list->string (reverse accum)) v (car l))
                   (cdr l))]))))
  (for/list ([line (in-list split-input)])
    (define indent (let loop ([line line] [i 0])
                     (cond
                       [(null? line) i]
                       [else
                        (define line-str (car line))
                        (cond
                          [(not (string? line-str)) i]
                          [(i . >= . (string-length line-str)) (loop (cdr line) i)]
                          [(char=? #\space (string-ref line-str i)) (loop line (+ i 1))]
                          [else i])])))
    (define all-indent (+ indent extra-indent))
    (paragraph plain
               (list
                (if (= all-indent 0) "" (hspace all-indent))
                (let loop ([line line] [indent indent])
                  (cond
                    [(not (eqv? indent 0))
                     (define line-str (car line))
                     (define len (string-length line-str))
                     (if (len . > . indent)
                         (loop (cons (substring line-str indent) (cdr line)) 0)
                         (loop (cdr line) (- indent len)))]
                    [(null? line) null]
                    [else
                     (cons (if (string? (car line))
                               (format-str (car line))
                               (car line))
                           (loop (cdr line) 0))]))))))

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

(define (call-with-discarded-output eval thunk)
  (define orig (call-in-sandbox-context eval current-output-port))
  (dynamic-wind
   (lambda ()
     (call-in-sandbox-context eval (lambda () (current-output-port (open-output-bytes)))))
   thunk
   (lambda ()
     (call-in-sandbox-context eval (lambda () (current-output-port orig))))))
