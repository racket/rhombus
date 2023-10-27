#lang racket/base
(require racket/port
         racket/interaction-info
         shrubbery/parse
         shrubbery/print
         (prefix-in rhombus: (submod "print.rkt" for-runtime))
         (submod "print.rkt" redirect)
         "syntax-parse-config.rkt"
         "rhombus-primitive.rkt")

(provide install-runtime-config!
         parameters)

;; Every parameter that's set in `install-runtmie-config`, needed in
;; "expand-config.rkt":
(define parameters
  (list current-interaction-info
        current-read-interaction
        print-boolean-long-form
        global-port-print-handler
        current-error-message-adjuster
        error-syntax->string-handler))

(define (install-runtime-config!)
  (current-interaction-info '#((submod rhombus reader)
                               get-interaction-info
                               #f))

  (current-read-interaction
   (lambda (src in)
     (when (terminal-port? in)
       (flush-output (current-output-port)))
     (define (ensure-count in)
       (if (port-counts-lines? in)
           in
           (let ([in (dup-input-port in)])
             (port-count-lines! in)
             in)))
     (parse-all (ensure-count in) #:source src #:mode 'interactive)))

  (print-boolean-long-form #t)

  (global-port-print-handler
   (let ([orig (global-port-print-handler)])
     (lambda (v op [mode 0])
       (if (racket-print-redirect? v)
           (orig (racket-print-redirect-val v) op 1)
           (rhombus:print v op 'expr #t)))))

  (current-error-message-adjuster
   (lambda (mode)
     (case mode
       [(contract)
        (lambda (str realm)
          (case realm
            [(racket/primitive)
             (values (with-handlers ([exn:fail:read? (lambda (exn) str)])
                       (define c (read (open-input-string str)))
                       (or (get-primitive-contract c)
                           (format "~s" c)))
                     'rhombus/primitive)]
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
                 [(regexp-match-positions #rx"^not a procedure;\n expected a procedure that can be applied to arguments" msg)
                  => (lambda (m)
                       (rhombus (string-append "not a function" (substring msg (cdar m)))))]
                 [(regexp-match-positions #rx"^index is out of range.*?vector:" msg)
                  => (lambda (m)
                       (rhombus (string-append (regexp-replace* #rx"vector" (substring msg (caar m) (cdar m)) "array")
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
     (define str (shrubbery-syntax->string s #:max-length len))
     (if (equal? str "")
         "[end of group]"
         str)))

  (config-syntax-parse!))
