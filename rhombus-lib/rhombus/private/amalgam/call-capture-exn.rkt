#lang racket/base
(require racket/string
         racket/treelist)

(provide call_capturing_exn
         call_capturing_values
         does_contain_each
         display_as_exn
         reindent_exn_msg)

(define (call_capturing_exn thunk capture-output? use-error-display?)
  (define s (and capture-output? (open-output-string)))
  (define (get-output) (and s (string->immutable-string (get-output-string s))))
  (let ([thunk (if capture-output?
                   (lambda ()
                     (parameterize ([current-output-port s])
                       (thunk)))
                   thunk)])
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (values #f
                               (cond
                                 [use-error-display?
                                  (define o (open-output-string))
                                  (parameterize ([current-error-port o]
                                                 [error-print-context-length 0]
                                                 [error-print-source-location #f])
                                    ((error-display-handler) (exn-message exn) exn))
                                  (get-output-string o)]
                                 [else
                                  (exn-message exn)])
                               exn
                               (get-output)))])
      (values (call-with-values
               (lambda ()
                 (call-with-continuation-prompt
                  thunk
                  (default-continuation-prompt-tag)))
               treelist)
              #f
              #f
              (get-output)))))

(define (call_capturing_values thunk)
  (call-with-values thunk treelist))

(define (does_contain_each strs in-str is-rx? rx-match?)
  (for/and ([pat (in-treelist strs)])
    (if (is-rx? pat)
        (rx-match? pat in-str)
        (does_contain pat in-str))))

(define (does_contain str in-str)
  (or (equal? str "")
      (let loop ([i 0])
        (cond
          [(< (- (string-length in-str) i)
              (string-length str))
           #f]
          [(and (eqv? (string-ref str 0)
                      (string-ref in-str i))
                (string=? str
                          (substring in-str i (+ i (string-length str)))))
           #t]
          [else
           (loop (add1 i))]))))

(struct exn:fail:test exn:fail (srcloc)
  #:property prop:exn:srclocs (lambda (e)
                                (list (exn:fail:test-srcloc e))))

(define (display_as_exn msg loc exn)
  (cond
    [loc
     ((error-display-handler) msg
                              (exn:fail:test msg
                                             (if (exn? exn)
                                                 (exn-continuation-marks exn)
                                                 (current-continuation-marks))
                                             loc))]
    [(exn? exn)
     ((error-display-handler) msg
                              (exn:fail msg
                                        (exn-continuation-marks exn)))]
    [else
     (displayln msg (current-error-port))]))

(define (reindent_exn_msg msg prefix-len)
  (define newline (string-append "\n" (make-string prefix-len #\space)))
  (define len (string-length msg))
  (define end (and (eqv? (string-ref msg (sub1 len)) #\newline)
                   (sub1 len)))
  (string-join (regexp-split #rx"\n" msg 0 end) newline))
