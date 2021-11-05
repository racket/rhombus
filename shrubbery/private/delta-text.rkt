#lang racket/base
(require racket/class)

(provide make-delta-text)

(define delta-text%
  (class object%
    (init-field next
                at-pos
                delta)

    (super-new)

    (define pre (if (negative? delta)
                    (+ at-pos delta)
                    at-pos))
    (define post (if (positive? delta)
                     (+ at-pos delta)
                     at-pos))
    
    (define/public (get-text s e)
      (cond
        [(e . <= . pre) (send next get-text s e)]
        [(or (s . >= . post)
             (and (negative? delta)
                  (s . >= . pre)))
         (send next get-text (- s delta) (- e delta))]
        [(negative? delta)
         (string-append
          (send next get-text s pre)
          (send next get-text (- pre delta) (- e delta)))]
        [else
         (string-append
          (if (s . < . pre)
              (send next get-text s pre)
              "")
          (make-string (min (- e pre) delta) #\space)
          (if (e . > . post)
              (send next get-text at-pos (- e delta))
              ""))]))

    (define/public (classify-position* pos)
      (define c
        (send next classify-position* (cond
                                        [(pos . < . pre) pos]
                                        [(or (pos . >= . post)
                                             (delta . < . 0))
                                         (- pos delta)]
                                        [else at-pos])))
      #;(log-error "[~s/~s] ~s = ~s" at-pos delta pos c)
      c)
    
    (define/public (classify-position pos)
      (define type (classify-position* pos))
      (if (hash? type) (hash-ref type 'type 'unknown) type))

    (define/public (get-token-range pos)
      (define-values (s e)
        (cond
          [(pos . < . pre)
           (define-values (s e) (send next get-token-range pos))
           (values s (shift-out e))]
          [(or (pos . >= . post)
               (negative? delta))
           (define-values (s e) (send next get-token-range (- pos delta)))
           (values (shift-out s) (+ e delta))]
          [else
           (define-values (s e) (send next get-token-range at-pos))
           (values (shift-out s) (shift-out e))]))
      #;(log-error "[~s/~s] ~s -> ~s ~s" at-pos delta pos s e)
      (values s e))

    (define/private (shift-in r)
      (if (r . <= . pre)
          r
          (- r delta)))

    (define/private (shift-out r)
      (and r
           (if (r . <= . pre)
               r
               (+ r delta))))
    
    (define/public (last-position)
      (shift-out (send next last-position)))

    (define/public (position-paragraph pos [eol? #f])
      (send next position-paragraph (shift-in pos) eol?))
    
    (define/public (paragraph-start-position para)
      (shift-out (send next paragraph-start-position para)))
    
    (define/public (paragraph-end-position para)
      (shift-out (send next paragraph-end-position para)))

    (define/public (backward-match pos cutoff)
      (shift-out (send next backward-match (shift-in pos) (shift-in cutoff))))

    (define/public (forward-match pos cutoff)
      (shift-out (send next forward-match (shift-in pos) (shift-in cutoff))))))

(define (make-delta-text t at-pos delta)
  (new delta-text%
       [next t]
       [at-pos at-pos]
       [delta delta]))

