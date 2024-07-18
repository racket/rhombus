#lang racket/base
(require racket/class
         "like-text.rkt"
         shrubbery/private/delta-text)

(define input
  (string-append "#! shrubbery\n"
                 "a\n"
                 " b\n"
                 "  c\n"
                 "   (d)\n"
                 "    @f{e}\n"))

(define (check rx delta)
  (define m (regexp-match-positions rx input))
  (define at-pos (caar m))
  (define input2
    (string-append (substring input 0 at-pos)
                   (if (positive? delta)
                       (make-string delta #\space)
                       "")
                   (substring input (if (positive? delta)
                                        at-pos
                                        (- at-pos delta)))))
  (define dt (make-delta-text (new like-text%
                                   [content input])
                              at-pos
                              delta))
  (define t (new like-text%
                 [content input2]))
  (for ([i (in-range (string-length input2))])
    (define-values (ds de) (send dt get-token-range i))
    (define-values (s e) (send t get-token-range i))
    (define dc (send dt classify-position i))
    (define c (send t classify-position i))
    (define dp (send dt position-paragraph i))
    (define p (send t position-paragraph i))
    (define dsp (send dt paragraph-start-position dp))
    (define sp (send t paragraph-start-position p))
    (define dep (send dt paragraph-end-position dp))
    (define ep (send t paragraph-end-position p))
    (define dbm (send dt backward-match i 0))
    (define bm (send t backward-match i 0))
    (unless (and (eqv? ds s)
                 (eqv? de e)
                 (equal? dc c)
                 (equal? dp p)
                 (equal? dsp sp)
                 (equal? dep ep)
                 (eqv? dbm bm))
      (error 'fail
             (string-append "range\n"
                            "  rx: ~s = ~s\n"
                            "  delta: ~s\n"
                            "  offset: ~a\n"
                            "  text: ~s\n"
                            "  delta t: ~s\n"
                            "  plain t: ~s")
             rx at-pos
             delta
             i
             (substring input2 i)
             (list ds de dc dp dsp dep dbm)
             (list s e c p sp ep bm)))))

(check #rx" b" 1)
(check #rx" b" -1)
(check #rx"  c" 1)
(check #rx"  c" -1)
(check #rx"  c" 2)
(check #rx"  c" -2)
(check #rx"  c" 5)
(check #rx"   .d" 1)
(check #rx"   .d" -1)
(check #rx"   .d" 2)
(check #rx"   .d" -2)
(check #rx"   .d" 3)
(check #rx"   .d" -3)
(check #rx"   .d" 5)
