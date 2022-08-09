#lang racket/base
(require shrubbery/srcloc
         shrubbery/property)

(provide syntax-srcloc
         no-srcloc
         span-srcloc
         relocate
         respan-empty
         respan
         with-syntax-error-respan)

(define (span-srcloc start end)
  (vector (syntax-source start)
          (syntax-line start)
          (syntax-column start)
          (syntax-position start)
          (let ([e (syntax-position end)]
                [s (syntax-position start)]
                [sp (syntax-span end)])
            (and s e sp
                 (max 0 (+ (- e s) sp))))))

(define (no-srcloc stx)
  (datum->syntax #f
                 (syntax-e stx)
                 #f
                 #f))

(define (relocate srcloc stx)
  (datum->syntax stx (syntax-e stx) srcloc (if (syntax? srcloc) srcloc stx)))

;; If the tail is empty, give it a source location
;; that matches the end of `op-stx`
(define (respan-empty op-stx tail)
  (cond
    [(or (null? tail)
         (and (syntax? tail)
              (null? tail)))
     (define o-loc (syntax-srcloc op-stx))
     (cond
       [o-loc
        (define pos (srcloc-position o-loc))
        (define span (srcloc-span o-loc))
        (cond
          [(and pos span)
           (datum->syntax #f '() (srcloc (srcloc-source o-loc)
                                         #f #f
                                         (+ pos span)
                                         0))]
          [else tail])]
       [else tail])]
    [else tail]))

(define (respan stx)
  (define e (syntax-e stx))
  (cond
    [(pair? e)
     (define head (car e))
     (define pos (syntax-position head))
     (cond
       [pos
        (define end-pos (let loop ([stx stx] [pos pos])
                          (cond
                            [(syntax? stx)
                             (define p (syntax-position stx))
                             (define sp (syntax-span stx))
                             (loop (syntax-e stx)
                                   (if (and p sp
                                            (equal? (syntax-source head)
                                                    (syntax-source stx)))
                                       (max pos (+ p sp))
                                       pos))]
                            [(pair? stx) (loop (cdr stx) (loop (car stx) pos))]
                            [else pos])))
        (datum->syntax stx
                       (syntax-e stx)
                       (vector (syntax-source head)
                               (syntax-line head)
                               (syntax-column head)
                               pos
                               (max 0 (- end-pos pos)))
                       stx)]
       [(eq? (syntax-e head) 'group)
        ;; this seems like a hack; maybe 'group should get a source location, too
        (define tail (datum->syntax #f (cdr e)))
        (define new-tail (respan tail))
        (if (eq? tail new-tail)
            stx
            (datum->syntax #f (cons head new-tail) new-tail))]
       [else stx])]
    [else stx]))

(define-syntax-rule (with-syntax-error-respan body ...)
  (call-with-syntax-error-respan
   (lambda ()
     body ...)))

(define (call-with-syntax-error-respan thunk)
  (with-handlers ([exn:fail:syntax?
                   (lambda (exn)
                     (define exprs (exn:fail:syntax-exprs exn))
                     (define new-exprs (map respan exprs))
                     (raise
                      (if (equal? exprs new-exprs)
                          exn
                          (struct-copy exn:fail:syntax exn
                                       [exprs new-exprs]))))])
    (thunk)))
