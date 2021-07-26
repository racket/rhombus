#lang racket/base

(provide span-srcloc
         relocate
         respan)

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

(define (relocate srcloc stx)
  (datum->syntax stx (syntax-e stx) srcloc stx))

(define (respan stx)
  (define head (car (syntax-e stx)))
  (define pos (syntax-position head))
  (cond
    [pos
     (define end-pos (let loop ([stx stx] [pos pos])
                       (cond
                         [(syntax? stx)
                          (define p (syntax-position stx))
                          (define sp (syntax-span stx))
                          (loop (syntax-e stx)
                                (if (and p sp)
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
    [else stx]))
