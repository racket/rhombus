#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/interaction-info
         shrubbery/print
         shrubbery/property
         racket/lazy-require
         (prefix-in rhombus: (submod "print.rkt" for-runtime))
         (submod "print.rkt" redirect)
         "rhombus-primitive.rkt"
         "normalize-syntax.rkt"
         "../version-case.rkt")

;; For `current-read-interaction` callback:
(lazy-require [shrubbery/parse (parse-all)]
              [racket/port (dup-input-port)])

(provide install-runtime-config!
         parameters)

(meta-if-version-at-least
 "8.13.0.4"
 (#%declare #:flatten-requires)
 (void))

;; TEMP accommodate Racket versions before `syntax/parse/report-config`
(meta-if-version-at-least
 "8.9.0.5"
 (require (only-in syntax/parse/report-config
                   current-report-configuration))
 (require (only-in racket/base
                   [void current-report-configuration])))

(meta-if-version-at-least
 "8.15.0.2"
 (void)
 (define error-syntax->name-handler (make-parameter (lambda (stx) #f))))

(define-syntax (define-install!+params stx)
  (syntax-parse stx
    #:literals (void)
    [(_ install! params
        (~and ((~or* void param) . _)
              form)
        ...)
     #'(begin
         (define (install!)
           form ...)
         (define params
           (list (~? param) ...)))]))

;; `parameters` is a list of all set parameters, needed in
;; "expand-config.rkt":
(define-install!+params install-runtime-config! parameters
  (current-interaction-info '#((submod rhombus reader)
                               get-interaction-info
                               #f))

  (current-read-interaction
   (lambda (src in)
     (when (terminal-port? in)
       (flush-output (current-output-port)))
     (define-values (line col pos) (port-next-location in))
     (parse-all in #:source src #:mode 'interactive
                #:start-column (or col 0))))

  (print-boolean-long-form #t)

  (global-port-print-handler
   (let ([orig (global-port-print-handler)])
     (lambda (v op [mode 0])
       (if (racket-print-redirect? v)
           ;; As we print unwrapped, we still want to go through
           ;; the port's print handler, so parameterize instead
           ;; of calling `orig` directly:
           (parameterize ([global-port-print-handler orig])
             (print (racket-print-redirect-val v) op mode))
           (rhombus:print v op 'expr #t)))))

  (current-error-message-adjuster
   (lambda (mode)
     (case mode
       [(contract)
        (lambda (str realm)
          (case realm
            [(racket/primitive)
             (with-handlers ([exn:fail:read? (lambda (exn)
                                               (values str realm))])
               (define c (read (open-input-string str)))
               (cond
                 [(get-primitive-contract c)
                  => (lambda (new-str)
                       (values new-str 'rhombus/primitive))]
                 [else (values str realm)]))]
            [else (values str realm)]))]
       [(message)
        (lambda (who who-realm msg msg-realm)
          (define-values (new-who new-who-realm)
            (case who-realm
              [(racket/primitive)
               (cond
                 [(get-primitive-who who)
                  => (lambda (new-who)
                       (values new-who 'rhombus/primitive))]
                 [else (values who who-realm)])]
              [else (values who who-realm)]))
          (define-values (new-msg new-msg-realm)
            (case who-realm
              [(racket/primitive)
               (define (rhombus s) (values s 'rhombus/primitive))
               (cond
                 [(regexp-match-positions #rx"^contract violation\n  expected: (.*)\n  given: (.*)" msg)
                  => (lambda (m)
                       (define expected (cadr m))
                       (define value (caddr m))
                       (rhombus (string-append "value does not satisfy annotation\n"
                                               "  annotation: " (substring msg (car expected) (cdr expected)) "\n"
                                               "  value: " (substring msg (car value) (cdr value)))))]
                 [(regexp-match-positions #rx"^not a procedure;\n expected a procedure that can be applied to arguments" msg)
                  => (lambda (m)
                       (rhombus (string-append "not a function" (substring msg (cdar m)))))]
                 [(regexp-match-positions #rx"^index is out of range.*?vector:" msg)
                  => (lambda (m)
                       (rhombus (string-append (regexp-replace* #rx"vector" (substring msg (caar m) (cdar m)) "array")
                                               (substring msg (cdar m)))))]
                 [(regexp-match-positions #rx"^index is out of range.*?treelist:" msg)
                  => (lambda (m)
                       (rhombus (string-append (regexp-replace* #rx"treelist" (substring msg (caar m) (cdar m)) "list")
                                               (substring msg (cdar m)))))]
                 [else (values msg msg-realm)])]
              [(racket)
               (define (rhombus s) (values s 'rhombus/primitive))
               (cond
                 [(regexp-match-positions #rx"^arity mismatch;\n the expected number of arguments does not match the given number" msg)
                  => (lambda (m)
                       (rhombus (string-append "wrong number of arguments in function call"
                                               (substring msg (cdar m)))))]
                 [else (values msg msg-realm)])]
              [else (values msg msg-realm)]))
          (values new-who new-who-realm new-msg new-msg-realm))]
       [else #f])))

  ;; for expand-time configure or syntax errors in the REPL
  (error-syntax->string-handler
   (lambda (s len)
     (define stx (normalize-syntax s))
     (define str (shrubbery-syntax->string stx #:max-length len))
     (if (equal? str "")
         "[end of group]"
         str)))

  (error-syntax->name-handler
   (lambda (s)
     (let name-of ([s s])
       (if (identifier? s)
           (if (syntax-raw-property s)
               (string->symbol (shrubbery-syntax->string s))
               (syntax-e s))
           (syntax-case* s (multi group op) (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
             [(group who . _) (name-of #'who)]
             [(multi (group who . _) . _) (name-of #'who)]
             [(op n) (name-of #'n)]
             [(head . _)
              ;; mainly intended to handle the not-a-shrubbery case:
              (and (identifier? #'head)
                   (name-of #'head))]
             [_ #f])))))

  (current-report-configuration
   (hasheq 'literal-to-what (lambda (v)
                              '("identifier" "identifiers"))
           'literal-to-string (lambda (v)
                                (format "`~s`" (if (syntax? v)
                                                   (syntax-e v)
                                                   v)))
           'datum-to-what (lambda (v)
                            (cond
                              [(symbol? v) '("identifier" "identifiers")]
                              [(keyword? v) '("keyword" "keywords")]
                              ;; `null` shows up with optional sequences
                              [(null? v) '("empty" "empty")]
                              [else '("literal" "literals")]))
           'datum-to-string (lambda (v)
                              (cond
                                [(symbol? v) (format "`~a`" (substring (format "~v" v) 2))]
                                [(keyword? v) (substring (format "~v" v) 2)]
                                [(null? v) "sequence"]
                                [else (format "~v" v)])))))
